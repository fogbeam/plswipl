#include "plswipl.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

PG_FUNCTION_INFO_V1(plswipl_handler);
PG_FUNCTION_INFO_V1(plswipl_inline);
PG_FUNCTION_INFO_V1(plswipl_validator);

PG_FUNCTION_INFO_V1(plswipl_function);

void _PG_init(void);

typedef struct {
    bool spi_pushed;
    fid_t fid;
    qid_t qid;
    term_t a0;
    int nargs, ninargs;
    char *argmodes;
    Oid *argtypes;
    Oid rettype;
    TypeFuncClass retclass;
    TupleDesc rettupledesc;
    MemoryContextCallback callback;
} plswipl_srfctx;

/* Global data */

static char *plswipl_argv[] = { "pl/swipl", "-p", "", "-f", "", NULL, };

void
_PG_init(void) {
    static bool inited = false;

    if (!inited) {
        char sharepath[MAXPGPATH];
	char *boot, *alias;
        int alias_size;
        char **p;

	get_share_path(my_exec_path, sharepath);

        alias_size = MAXPGPATH + 20;
        alias = (char *) palloc(alias_size);
        snprintf(alias, alias_size, "plswipl=%s/plswipl", sharepath);
        plswipl_argv[2] = alias;

	boot = (char *) palloc(MAXPGPATH);
	snprintf(boot, MAXPGPATH, "%s/plswipl/boot.prolog", sharepath);
        plswipl_argv[4] = boot;

        printf("swi-prolog args:");
        for (p = plswipl_argv; *p; p++) printf(" \"%s\"", *p);
        printf(".\n");

        if (!PL_initialise(5, plswipl_argv)) {
            PL_halt(1);
            elog(ERROR, "PL_initialise failed");
        }

        PL_register_extensions_in_module("spi", plswipl_spi_extension);

        inited = 1;
    }
}

Datum
plswipl_handler(PG_FUNCTION_ARGS) {
    Datum retval;

    if (CALLED_AS_TRIGGER(fcinfo)) {
        printf("function called as trigger!\n"); fflush(stdout);
        /*
         * Called as a trigger procedure
         */
        /* TriggerData    *trigdata = (TriggerData *) fcinfo->context; */
        exit(-1);
    }
    else {
        /*
         * Called as a function
         */

        retval = plswipl_function(fcinfo);
    }
    return retval;
}

static void
check_exception(qid_t qid, char *context) {
    term_t e = PL_exception(qid);
    if (e) {
        char *e_chars = NULL;
        if (PL_get_chars(e, &e_chars, CVT_ALL|CVT_VARIABLE|CVT_WRITE|BUF_RING|REP_UTF8))
            ereport(ERROR,
                    (errcode(ERRCODE_EXTERNAL_ROUTINE_EXCEPTION),
                     errmsg("exception %i %s", (int)e, (e_chars ? e_chars : "*UNKNOWN*")),
                     errcontext("%s", context)));
    }
}

Datum
plswipl_inline(PG_FUNCTION_ARGS) {
    InlineCodeBlock *codeblock = (InlineCodeBlock *) PG_GETARG_POINTER(0);
    static predicate_t predicate_handle_do = 0;
    fid_t fid;
    term_t a0;

    if (!predicate_handle_do)
        predicate_handle_do = PL_predicate("handle_do", 1, "plswipl_low");

    fid = PL_open_foreign_frame();
    a0 = PL_new_term_refs(1);
    if (PL_put_string_chars(a0, codeblock->source_text)) {
        qid_t qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, predicate_handle_do, a0);
        if (!PL_next_solution(qid))
            check_exception(qid, "while executing DO with PLSWIPL");
        PL_close_query(qid);
    }
    PL_discard_foreign_frame(fid);
    PG_RETURN_VOID();
}

Datum
plswipl_validator(PG_FUNCTION_ARGS) {
    PG_RETURN_VOID();
}

static int
cons_functor_chars(term_t out, const char *name, int arity, term_t args) {
    atom_t atom_name = PL_new_atom(name);
    int r = PL_cons_functor_v(out, PL_new_functor(atom_name, arity), args);
    PL_unregister_atom(atom_name);
    return r;
}

static char *
utf_e2u(const char *str) {
    printf("database encoding: %d [ascii: %d]\n", GetDatabaseEncoding(),PG_SQL_ASCII); fflush(stdout); 
    return pg_server_to_any(str, strlen(str), PG_UTF8);
}

static Datum
ebad_term_to_datum_conversion(term_t t, Oid type) {
    char *str;
    if (!PL_get_chars(t, &str, (CVT_ALL|CVT_VARIABLE|CVT_WRITEQ|BUF_RING|REP_UTF8) & ~CVT_LIST))
        str = "***unwritable***";
    elog(ERROR,
         "Cannot convert prolog term '%s' (%d) to PostgreSQL type %s (%d)",
         str, PL_term_type(t), format_type_be(type), type);
    return 0; /* unreachable */
}

static Datum plswipl_term_to_datum(term_t t, Oid type);
static void plswipl_datum_to_term(Oid type, Datum datum, term_t t);

static Datum
plswipl_term_to_datum_array(term_t t, Oid type, Oid elemtype) {
    size_t length;
    if (PL_skip_list(t, 0, &length) == PL_LIST) {
        ArrayBuildState *astate;
        int ndims, i;
        int dims[MAXDIM];
        int lbs[MAXDIM];
        Assert(elemtype != InvalidOid);
        astate = initArrayResult(elemtype, CurrentMemoryContext, true);
        memset(dims, 0, sizeof(dims));
        memset(lbs, 0, sizeof(lbs));
        if (length == 0) {
            ndims = 0;
        }
        else {
            term_t h = PL_new_term_ref();
            term_t l = PL_copy_term_ref(t);
            
            for (i = 0; i < length; i++) {
                if (!PL_get_list(l, h, l))
                    Assert(false);
                accumArrayResult(astate, plswipl_term_to_datum(h, elemtype), false, elemtype, CurrentMemoryContext);
            }
            ndims = 1;
            dims[0] = length;
            for (i = 0; i < ndims; i++) lbs[i] = 1;
        }
        return makeMdArrayResult(astate, ndims, dims, lbs,
                                 CurrentMemoryContext, true);
    }
    
    return ebad_term_to_datum_conversion(t, type);
}

static Datum
plswipl_term_to_datum(term_t t, Oid type) {
    switch(type) {
    case BOOLOID: {
        int v;
        if (PL_get_bool(t, &v))
            return BoolGetDatum(v);
        break;
    }
    case OIDOID:
    case INT2OID:
    case INT4OID:
    case INT8OID: {
        int64_t v;
        if (PL_get_int64(t, &v)) {
            if (type == INT2OID) {
                if (((uint64_t)v >> 16) == 0)
                    return Int16GetDatum(v);
            }
            else if (type == INT4OID) {
                if (((uint64_t)v >> 32) == 0)
                    return Int32GetDatum(v);
            }
            else if (type == OIDOID) {
                if (((uint64_t)v >> 32) == 0)
                    return ObjectIdGetDatum(v);
            }
            else return Int64GetDatum(v);
        }
        break;
    }
    case FLOAT4OID:
    case FLOAT8OID: {
        double v;
        if (PL_get_float(t, &v)) {
            if (type == FLOAT4OID)
                return Float4GetDatum(v);
            return Float8GetDatum(v);
        }
        break;
    }
        
    case TEXTOID: {
        char *v;
        if (PL_get_chars(t, &v, (CVT_ALL|CVT_WRITE|BUF_RING|REP_UTF8) & ~CVT_LIST))
            return PointerGetDatum(DirectFunctionCall1(textin, PointerGetDatum(v)));
    }

    default: {
        Oid elemtype = get_element_type(type);
        if (elemtype != InvalidOid)
            return plswipl_term_to_datum_array(t, type, elemtype);
    }
    }
    
    return ebad_term_to_datum_conversion(t, type);
}

static void
plswipl_datum_array_to_term_recurse(Oid elemtype, Datum **elems, int ndims, int *dims, term_t t) {
    term_t a = PL_new_term_ref();
    printf("elemtype: %d, elems: %p, ndims: %d, dims[0]: %d, t: %ld\n",
           elemtype, elems, ndims, dims[0], t); fflush(stdout);
    PL_put_nil(t);
    if (ndims) {
        int i;
        for (i = 0; i < dims[0]; i++) {
            if (ndims > 1) {
                printf("recursing with ndims: %d\n", ndims - 1); fflush(stdout);
                plswipl_datum_array_to_term_recurse(elemtype, elems, ndims - 1, dims + 1, a);
            }
            else {
                Datum elem = *(--*elems);
                plswipl_datum_to_term(elemtype, elem, a);
                /* PL_put_atom_chars(a, "foo"); */
                /* PL_put_integer(a, 72); */
            }
            if (!PL_cons_list(t, a, t))
                goto error;
        }
    }
    {
        char *str;
        if (PL_get_chars(t, &str, (CVT_ALL|CVT_VARIABLE|CVT_WRITE|BUF_RING|REP_UTF8) & ~CVT_STRING))
            printf("in prolog term %ld: %s\n", t, str); fflush(stdout);
        return;
    }
        
  error:
    elog(ERROR, "unable to convert PostgreSQL array to Prolog list");
}

static void
plswipl_datum_array_to_term(Oid type, Oid elemtype, Datum datum, term_t t) {
    ArrayType *arrtype = DatumGetArrayTypeP(datum);
    if (arrtype) {
	int16 elemlen;
        bool elembyval;
        char elemalign;
        Datum *elems, *tail;
        int nelems;
        int ndims = ARR_NDIM(arrtype);
        int *dims = ARR_DIMS(arrtype);
        Assert(elemtype == ARR_ELEMTYPE(arrtype));
        get_typlenbyvalalign(elemtype, &elemlen, &elembyval, &elemalign);
        deconstruct_array(arrtype, elemtype, elemlen, elembyval, elemalign,
                          &elems, NULL, &nelems);
        tail = elems + nelems;
        printf("array arrtype: %p, elemtype: %d, elemlen: %d, elembyval: %d, elemalign: %d, elems: %p, nelems: %d, tail: %p\n",
               arrtype, elemtype, elemlen, elembyval, elemalign, elems, nelems, tail); fflush(stdout);
        plswipl_datum_array_to_term_recurse(elemtype, &tail, ndims, dims, t);
        return;
    }
    elog(ERROR, "Cannot retrieve ArrayType for type %s (%d)",
         format_type_be(type), type);
}

static void
plswipl_datum_to_term(Oid type, Datum datum, term_t a) {
    printf("converting element of type %s (%d) to prolog\n", format_type_be(type), type); fflush(stdout);
    switch(type) {
    case BOOLOID:
        if (PL_put_bool(a, DatumGetBool(datum))) return;
        break;
    case INT2OID:
        if (PL_put_integer(a, DatumGetInt16(datum))) return;
        break;
    case INT4OID:
        if (PL_put_integer(a, DatumGetInt32(datum))) return;
        break;
    case INT8OID:
        if (PL_put_int64(a, DatumGetInt64(datum))) return;
        break;
    case FLOAT4OID:
        if (PL_put_float(a, DatumGetFloat4(datum))) return;
        break;
    case FLOAT8OID:
        if (PL_put_float(a, DatumGetFloat8(datum))) return;
        break;
    case TEXTOID:
        if (PL_put_string_chars(a, utf_e2u(TextDatumGetCString(datum)))) return;
        break;
    default: {
        Oid elemtype = get_element_type(type);
        if (elemtype != InvalidOid) {
            plswipl_datum_array_to_term(type, elemtype, datum, a);
            return;
        }
    }
    }

    elog(ERROR, "PL/SWI-Prolog cannot convert PostgreSQL value of type %s (%d) to a Prolog term",
         format_type_be(type), type);
}


static void
plswipl_clean_context(plswipl_srfctx *srfctx) {
    printf("plswipl_clean_context: %p, fid: %li, qid: %li\n",
           srfctx, srfctx->fid, srfctx->qid); fflush(stdout);
    if (srfctx->spi_pushed)
        SPI_pop();
    if (srfctx->fid) {
        if (srfctx->qid)
            PL_close_query(srfctx->qid);
        PL_close_foreign_frame(srfctx->fid);
    }
}

Datum
plswipl_function(PG_FUNCTION_ARGS) {
    Oid fn_oid = fcinfo->flinfo->fn_oid;
    ReturnSetInfo *rsi = (ReturnSetInfo *)fcinfo->resultinfo;
    HeapTuple procTup;
    Form_pg_proc procStruct;
    Datum proallargtypes, proargmodes, prosrcdatum;
    Oid *argtypes, rettype;
    char *argmodes;
    char *proSource;
    bool isNull, ok;
    int nargs, ninargs, noutargs, i, j;
    fid_t fid;
    term_t a0, a1;
    qid_t qid;
    FuncCallContext  *funcctx = NULL;
    TupleDesc rettupledesc;
    TypeFuncClass retclass;

    static predicate_t predicate_handle_function = 0;
    if (!predicate_handle_function)
        predicate_handle_function = PL_predicate("handle_function", 2, "plswipl_low");

    printf("fcinfo: %p, context: %p, resultinfo: %p, fncollation: %i, isnull: %d, nargs: %d\n",
           fcinfo, fcinfo->context, rsi, fcinfo->fncollation, fcinfo->isnull, fcinfo->nargs);
    if (rsi)
        printf("ReturnSetInfo: %p, type: %i, econtext: %p, expectedDesc: %p, allowedModes: %i, "
               "returnMode: %i, isDone: %i, setResult: %p, setDesc: %p\n",
               rsi, rsi->type, rsi->econtext, rsi->expectedDesc, rsi->allowedModes,
               rsi->returnMode, rsi->isDone, rsi->setResult, rsi->setDesc);
    fflush(stdout);

    if (SRF_IS_FIRSTCALL()) {
        plswipl_srfctx *srfctx;
        MemoryContext oldcontext = NULL;

        procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fn_oid));
        if (!HeapTupleIsValid(procTup))
            elog(ERROR, "cache lookup failed for function %u", fn_oid);
        procStruct = (Form_pg_proc) GETSTRUCT(procTup);
        nargs = procStruct->pronargs;

        proallargtypes = SysCacheGetAttr(PROCOID, procTup,  Anum_pg_proc_proallargtypes, &isNull);
        if (isNull) {
            if (procStruct->proargtypes.dim1 != nargs) elog(ERROR, "size mismatch between function arguments and type array");
            argtypes = procStruct->proargtypes.values;
        }
        else {
            ArrayType *arr = DatumGetArrayTypeP(proallargtypes);
            if ((ARR_NDIM(arr) != 1)       ||
                (ARR_DIMS(arr)[0] < nargs) ||
                ARR_HASNULL(arr)           ||
                (ARR_ELEMTYPE(arr) != OIDOID)) elog(ERROR, "proallargtypes is not a 1-D Oid array");
            nargs = ARR_DIMS(arr)[0];
            argtypes = (Oid*)ARR_DATA_PTR(arr);
        }

        proargmodes = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_proargmodes, &isNull);
        if (isNull)
            argmodes = NULL;
        else {
            ArrayType *arr = DatumGetArrayTypeP(proargmodes);
            if ((ARR_NDIM(arr) != 1 )       ||
                (ARR_DIMS(arr)[0] != nargs) ||
                ARR_HASNULL(arr)            ||
                (ARR_ELEMTYPE(arr) != CHAROID)) elog(ERROR, "proargmodes is not a 1-D char array");
            argmodes = (char*)ARR_DATA_PTR(arr);
        }

        if (procStruct->proretset) {
            funcctx = SRF_FIRSTCALL_INIT();
            oldcontext = MemoryContextSwitchTo(funcctx->multi_call_memory_ctx);
        }

        srfctx =(plswipl_srfctx*)palloc(sizeof(plswipl_srfctx));
        srfctx->spi_pushed = 0;
        srfctx->qid = 0;
        srfctx->callback.arg = (void *)srfctx;
        srfctx->callback.func = (MemoryContextCallbackFunction)plswipl_clean_context;

        retclass = get_call_result_type(fcinfo, &rettype, &rettupledesc);

        fid = PL_open_foreign_frame();
        srfctx->fid = fid;

        MemoryContextRegisterResetCallback(CurrentMemoryContext, &(srfctx->callback));

        a0 = PL_new_term_refs(nargs + 1);

        for (j = i = 0; i < nargs; i++) {
            char argmode = (argmodes ? argmodes[i] : 'i');
            term_t a = a0 + i;
            switch (argmode) {
            case 'i':
                if (fcinfo->argnull[j])
                    PL_put_nil(a);
                else {
                    plswipl_datum_to_term(argtypes[i], fcinfo->arg[j], a);
                    j++;
                }
            case 'o':
                /* output: do nothing yet! */
                break;
            default:
                ereport(ERROR,
                        (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                         errmsg("PL/SWI-Prolog functions cannot accept argument mode '%c'",
                                argmode)));
            }
        }
        ninargs = j;
        noutargs = nargs - ninargs;
        if ((noutargs == 0) && (rettype != VOIDOID)) {
            nargs++;
            noutargs++;
        }

        a1 = PL_new_term_refs(2);
        if (!cons_functor_chars(a1 + 0, procStruct->proname.data, nargs, a0)) {
            ereport(ERROR,
                    (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                     errmsg("PL/SWI-Prolog PL_cons_functor_v failed")));
        }

        prosrcdatum = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_prosrc, &isNull);
        if (isNull)
            elog(ERROR, "null prosrc");
        proSource = TextDatumGetCString(prosrcdatum);
        if (!PL_put_string_chars(a1 + 1, proSource)) {
            ereport(ERROR,
                    (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                     errmsg("PL/SWI-Prolog PL_put_string_chars failed")));
        }

        if (procStruct->proretset) {
            funcctx->user_fctx = srfctx;
            srfctx->a0 = a0;
            srfctx->nargs = nargs;
            srfctx->ninargs = ninargs;
            srfctx->rettupledesc = rettupledesc;
            if (argmodes) {
                srfctx->argmodes = pnstrdup(argmodes, nargs);
                srfctx->argtypes = palloc(nargs * sizeof(Oid));
                memcpy(srfctx->argtypes, argtypes, nargs * sizeof(Oid));
            }
            else {
                srfctx->argmodes = NULL;
                srfctx->argtypes = NULL;
            }
            srfctx->rettype = rettype;
            MemoryContextSwitchTo(oldcontext);
        }

        ReleaseSysCache(procTup);

        qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, predicate_handle_function, a1);
        srfctx->qid = qid;

        /*
          SPI_push();
          srfctx->spi_pushed = 1;
        */
    }
    else {
        plswipl_srfctx *srfctx;
        funcctx = SRF_PERCALL_SETUP();
        srfctx = (plswipl_srfctx*)funcctx->user_fctx;
        fid = srfctx->fid;
        qid = srfctx->qid;
        a0 = srfctx->a0;
        nargs = srfctx->nargs;
        ninargs = srfctx->ninargs;
        noutargs = nargs - ninargs;
        argmodes = srfctx->argmodes;
        argtypes = srfctx->argtypes;
        rettype = srfctx->rettype;
        rettupledesc = srfctx->rettupledesc;
        printf("SRF again!\n");
    }

    printf("fid: %li, qid: %li, a0: %li, nargs: %i, argmodes: %s, argtypes: %p, rettype: %i\n",
           fid, qid, a0, nargs, argmodes, argtypes, rettype); fflush(stdout);

    ok = PL_next_solution(qid);
    if (ok) {
        Datum out;
        if (noutargs == 1) {
            int i;
            if (argmodes) {
                for (i = 0; (i < nargs - 1) && (argmodes[i] == 'i'); i++);
            }
            else
                i = nargs - 1;
            out = plswipl_term_to_datum(a0 + i, rettype);
        }
        else if (noutargs > 1) {
            Datum *outs = (Datum *)palloc(sizeof(Datum) * noutargs);
            bool *isnulls = (bool *)palloc(sizeof(bool) * noutargs);
            Assert(rettupledesc != NULL);
            Assert(argmodes != NULL);
            for (j = i = 0; i < nargs; i++) {
                if (argmodes[i] == 'o') {
                    outs[j] = plswipl_term_to_datum(a0 + i, rettupledesc->attrs[j]->atttypid);
                    isnulls[j] = 0;
                    j++;
                }
            }
            Assert(j == natts);
            out = HeapTupleGetDatum(heap_form_tuple(rettupledesc, outs, isnulls));
            pfree(outs);
            pfree(isnulls);
        }
        else { /* void function */
            out = (Datum) 0;
        }

        if (funcctx)
            SRF_RETURN_NEXT(funcctx, out);

        return out;
    }

    /* so, not ok! */
    check_exception(qid, "while callin PLSWIPL function");
    if (funcctx)
        SRF_RETURN_DONE(funcctx);

    ereport(ERROR,
            (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
             errmsg("PL/SWI-Prolog function failed")));
}
