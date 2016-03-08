#include "postgres.h"
#include "fmgr.h"
#include "utils/guc.h"
#include "commands/trigger.h"
#include "miscadmin.h"
#include "port.h"
#include "utils/elog.h"
#include "utils/syscache.h"
#include "catalog/pg_proc.h"
#include "access/htup_details.h"

#include <SWI-Prolog.h>

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

PG_FUNCTION_INFO_V1(plswipl_handler);
PG_FUNCTION_INFO_V1(plswipl_inline);
PG_FUNCTION_INFO_V1(plswipl_validator);

PG_FUNCTION_INFO_V1(plswipl_function);

void _PG_init(void);

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
        
        if (PL_initialise(5, plswipl_argv)) {
            inited = 1;
            return;
        }
        PL_halt(1);
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
        char *e_chars;
        if (!PL_get_chars(e, &e_chars, CVT_ALL|CVT_VARIABLE|CVT_WRITE|BUF_MALLOC|REP_UTF8))
            e_chars = NULL;
        ereport(ERROR,
                (errcode(ERRCODE_EXTERNAL_ROUTINE_EXCEPTION),
                 errmsg("exception %i %s", (int)e, (e_chars ? e_chars : "*UNKNOWN*")),
                 errcontext("%s", context)));
            if (e_chars) PL_free(e_chars);
    }
}

Datum
plswipl_inline(PG_FUNCTION_ARGS) {
    InlineCodeBlock *codeblock = (InlineCodeBlock *) PG_GETARG_POINTER(0);
    static predicate_t p = 0;
    fid_t fid;
    term_t a0;

    if (!p)
        p = PL_predicate("do", 1, "plswipl_low");
    
    fid = PL_open_foreign_frame();
    a0 = PL_new_term_refs(1);
    if (PL_put_string_chars(a0, codeblock->source_text)) {
        qid_t qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, p, a0);
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

void
plswipl_dump_info(PG_FUNCITON_ARGS) {
    FmgrInfo *info;
    HeapTuple	procTup;
    Oid fn_oid = fcinfo->flinfo->fn_oid;
    Form_pg_proc procStruct;
    HeapTupleHeader tupData;
    
    printf("flinfo: %p, context: %p, resultinfo: %p, oid: %i, isnull: %i, nargs: %i\n",
           fcinfo->flinfo,
           fcinfo->context,
           fcinfo->resultinfo,
           fcinfo->fncollation,
           fcinfo->isnull,
           fcinfo->nargs);

    info = fcinfo->flinfo;
    if (info) {
        printf("fn_addr: %p, oid: %i, nargs: %i, strict: %i, retset: %i, stats: %i, extra: %p\n",
               info->fn_addr,
               info->fn_oid,
               info->fn_nargs,
               info->fn_strict,
               info->fn_retset,
               info->fn_stats,
               info->fn_extra);
    }

    procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fn_oid));
    if (!HeapTupleIsValid(procTup))
        elog(ERROR, "cache lookup failed for function %u", fn_oid);

    printf("procTup(%p): len: %i, self.blkid: (%i,%i), self.posid: %i, tableOid: %i, data: %p\n",
           procTup, procTup->t_len, procTup->t_self.ip_blkid.bi_hi, procTup->t_self.ip_blkid.bi_lo,
           procTup->t_self.ip_posid, procTup->t_tableOid, procTup->t_data);

    tupData = procTup->t_data;
    printf("tupData(%p): ctid.ip_blkid: (%i, %i), ctid.ip_podid: %i, infomask2: %i, infomask: %i, hoff: %i\n",
           tupData, tupData->t_ctid.ip_posid, tupData->t_ctid.ip_blkid.bi_hi, tupData->t_ctid.ip_blkid.bi_lo,
           tupData->t_infomask2, tupData->t_infomask, tupData->t_hoff);
    
    procStruct = (Form_pg_proc) GETSTRUCT(procTup);

    printf("procStruct: proname: %s, pronamespace: %i, proowner: %i, prolang: %i, procost: %f, prorows: %f, provariadic: %i, protransform: %i, "
           "proisagg: %i, proiswindow: %i, prosecdef: %i, proleakproof: %i, proisstrict: %i, proretset: %i, "
           "provolatile: %i, proparallel: %%i, pronargs: %i, pronargdefauls: %i, prorettype: %i\n",
           procStruct->proname.data,
           procStruct->pronamespace,
           procStruct->proowner,
           procStruct->prolang,
           procStruct->procost,
           procStruct->prorows,
           procStruct->provariadic,
           procStruct->protransform,
           procStruct->proisagg,
           procStruct->proiswindow,
           procStruct->prosecdef,
           procStruct->proleakproof,
           procStruct->proisstrict,
           procStruct->proretset,
           procStruct->provolatile,
           /* procStruct->proparallel, */
           procStruct->pronargs,
           procStruct->pronargdefaults,
           procStruct->prorettype);

}

Datum
plswipl_function(PG_FUNCTION_ARGS) {
    Oid fn_oid = fcinfo->flinfo->fn_oid;
    HeapTuple procTup;
    Form_pg_proc procStruct;
    Datum proallargtypes;
    Datum proargmodes;
    bool isNull;
    int numargs, i;
    fid_t fid;
    
    procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fn_oid));
    if (!HeapTupleIsValid(procTup))
        elog(ERROR, "cache lookup failed for function %u", fn_oid);
    procStruct = (Form_pg_proc) GETSTRUCT(procTup);
    nargs = procStruct->procnargs;
    
    proallargtypes = SysCacheGetAttr(PROCOID, procTup,  Anum_pg_proc_proallargtypes, &isNull);
    if (isNull) {
        if (procStruct->proargtypes.dim1 != nargs) elog(ERROR, "size mismatch between function arguments and type array");
        argtypes = procStruct->proargtypes.values;
    }
    else {
        ArrayType *arr = DatumGetArrayTypeP(proallargtypes);
        if ((ARG_NDIM(arr) != 1)       ||
            (ARR_DIMS(arr)[0] < nargs) ||
            ARR_HASNULL(arr)           ||
            (ARR_ELEMTYPE(arr) != OIDOID)) elog(ERROR, "proallargtypes is not a 1-D Oid array");
        nargs = ARR_DIMS(arr)[0];
        argtypes = ARR_DATA_PTR(arr);
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
        argmodes = ARR_DATA_PTR(arr);
    }

    fid = PL_open_foreign_frame();
    a0 = PL_new_term_refs(nargs);
    
    for (i = 0; i < nargs; i++) {
        if (!argmodes || (argmodes[i] == 'i')) {
            HeapTuple argTypeTup;
            Form_pg_type argTypeStruct;
            Datum *datum = fcinfo->arg + i;
            argTypeTup = SearchSysCache1(TYPEOID, ObjectIdGetDatum(argtypes[i]));
            if (!HeapTupleIsValid(argTypeTup))
                elog(ERROR, "cache lookup failed for type %u", argtypes[i]);
            argTypeStruct = (Form_pg_type) GETSTRUCT(argTypeTup);
            switch (argTypeStruct->typtype) {
            case TYPTYPE_PSEUDO:
            case TYPTYPE_COMPOSITE:
                /* Disallow pseudotype and composite arguments */
                PL_discard_foreign_frame(fid);
                ereport(ERROR,
                        (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
                         errmsg("PL/SWI-Prolog functions cannot accept type %s",
                                format_type_be(types[i]))));
                break;
            default:
                switch(typeOid) {
                case BOOLOID:
                case INT2OID:
                    PL_put_integer(a0, i, DatumGetInt16(*datum));
                    break;
                case INT4OID:
                    PL_put_integer(a0, i, DatumGetIn32(*datum));
                    break;
                case INT8OID:
                    PL_put_integer(a0, i, DatumGetIn64(*datum));
                    break;
                }
            }
        }

// WORKING HERE!!!
        
    proallarg
    if  (procStruct->pronargs) {
        Oid *types;
        char **names, *modes;
        int pos, total;

        
        
        total = get_func_arg_info(procTup, &types, &names, &modes);



        
        nargs = procStruct->pronargs;
        prosrcdatum = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_prosrc, &isnull);
        if (isnull)
            elog(ERROR, "null prosrc");
        procSource = TextDatumGetCString(prosrcdatum);
        fid = PL_open_foreign_frame();
        if (!p)
            p = PL_predicate("handle", 7, "plswipl_low")
    }

    
    fflush(stdout);
    PG_RETURN_VOID();
}
