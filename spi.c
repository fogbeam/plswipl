#include "plswipl.h"

static char *
rc2str(int rc) {
    switch (rc) {
    case SPI_ERROR_CONNECT:
        return "spi_error_connect";
    case SPI_ERROR_COPY:
        return "spi_error_copy";
    case SPI_ERROR_OPUNKNOWN:
        return "spi_error_opunknown";
    case SPI_ERROR_UNCONNECTED:
        return "spi_error_unconnected";
    case SPI_ERROR_ARGUMENT:
        return "spi_error_argument";
    case SPI_ERROR_PARAM:
        return "spi_error_param";
    case SPI_ERROR_TRANSACTION:
        return "spi_error_transaction";
    case SPI_ERROR_NOATTRIBUTE:
        return "spi_error_noattribute";
    case SPI_ERROR_NOOUTFUNC:
        return "spi_error_nooutfunc";
    case SPI_ERROR_TYPUNKNOWN:
        return "spi_error_typunknown";
    case SPI_OK_CONNECT:
        return "spi_ok_connect";
    case SPI_OK_FINISH:
        return "spi_ok_finish";
    case SPI_OK_FETCH:
        return "spi_ok_fetch";
    case SPI_OK_UTILITY:
        return "spi_ok_utility";
    case SPI_OK_SELECT:
        return "spi_ok_select";
    case SPI_OK_SELINTO:
        return "spi_ok_selinto";
    case SPI_OK_INSERT:
        return "spi_ok_insert";
    case SPI_OK_DELETE:
        return "spi_ok_delete";
    case SPI_OK_UPDATE:
        return "spi_ok_update";
    case SPI_OK_CURSOR:
        return "spi_ok_cursor";
    case SPI_OK_INSERT_RETURNING:
        return "spi_ok_insert_returning";
    case SPI_OK_DELETE_RETURNING:
        return "spi_ok_delete_returning";
    case SPI_OK_UPDATE_RETURNING:
        return "spi_ok_update_returning";
    case SPI_OK_REWRITTEN:
        return "spi_ok_rewritten";
    default:
        if (rc < 0)
            return "spi_error_unknown";
        else
            return "spi_ok_unknown";
    }
}

static int
PL_get_chars_ex(term_t a, char **out, int flags) {
    return PL_get_chars(a, out, flags | CVT_EXCEPTION);
}

static int
rc2ex(int rc) {
    if (rc < 0) {
        term_t e = PL_new_term_ref();
        if (!PL_unify_term(e,
                           PL_FUNCTOR_CHARS, "spi_error", 2,
                           PL_INT, rc,
                           PL_UTF8_CHARS, rc2str(rc)))
            elog(ERROR, "PL_unify_term failed");
        PL_throw(e);
        assert(0); /* unreachable */
    }
    return rc;
}

static int
unify_rc(term_t a, int rc) {
    rc2ex(rc);
    return PL_unify_atom_chars(a, rc2str(rc));
}

static foreign_t
my_connect(term_t a0, int arity, void* context) {
    assert(arity == 0);
    rc2ex(SPI_connect());
    PL_succeed;
}

static foreign_t
my_finish(term_t a0, int arity, void *context) {
    assert(arity == 0);
    rc2ex(SPI_finish());
    PL_succeed;
}

static foreign_t
my_execute(term_t a0, int arity, void *context) {
    char *command;
    int read_only;
    long count;
    int rc;
    assert(arity == 4);
    PL_get_chars_ex(a0,     &command, CVT_ALL|CVT_EXCEPTION|BUF_RING|REP_UTF8);
    PL_get_bool_ex (a0 + 1, &read_only);
    PL_get_long_ex (a0 + 2, &count);
    rc = SPI_execute(command, read_only, count);
    return unify_rc(a0 + 3, rc);
}

static foreign_t
my_get_tuples(term_t a0, int arity, void *context) {
    SPITupleTable *tuptable = SPI_tuptable;
    ssize_t j = SPI_processed;
    term_t l1 = PL_new_term_ref();
    PL_put_nil(l1);
    if (tuptable != NULL) {
        term_t l2 = PL_new_term_ref();
        TupleDesc tupdesc = tuptable->tupdesc;
        printf("converting resultset to prolog, rows: %ld, cols: %d\n",
               j, tupdesc->natts); fflush(stdout);
        while (j--) {
            HeapTuple tuple = tuptable->vals[j];
            plswipl_tuple_to_term(tupdesc, tuple, l2);
            if (!PL_cons_list(l1, l2, l1)) goto error;
        }
    }
    return PL_unify(l1, a0);

error:
    elog(ERROR, "unable to construct resultset object");
}

static foreign_t
my_get_head(term_t a0, int arity, void *context) {
    term_t t = PL_new_term_ref();
    plswipl_tuplename_to_term(SPI_tuptable->tupdesc, t);
    return PL_unify(a0, t);
}

static foreign_t
my_processed(term_t a0, int arity, void *context) {
    assert(arity == 1);
    return PL_unify_int64(a0, SPI_processed);
}

PL_extension
plswipl_spi_extension[] = { { "connect",    0, my_connect,    PL_FA_VARARGS },
                            { "finish",     0, my_finish,     PL_FA_VARARGS },
                            { "execute",    4, my_execute,    PL_FA_VARARGS },
                            { "processed",  1, my_processed,  PL_FA_VARARGS },
                            { "get_tuples", 1, my_get_tuples, PL_FA_VARARGS },
                            { "get_head",   1, my_get_head,   PL_FA_VARARGS },
                            { NULL,          0, NULL,       0 } };

