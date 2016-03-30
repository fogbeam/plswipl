#include "postgres.h"
#include "fmgr.h"
#include "funcapi.h"
#include "port.h"
#include "miscadmin.h"
#include "access/htup_details.h"
#include "catalog/pg_proc.h"
#include "catalog/pg_type.h"
#include "commands/trigger.h"
#include "executor/spi.h"
#include "mb/pg_wchar.h"
#include "utils/palloc.h"
#include "utils/guc.h"
#include "utils/elog.h"
#include "utils/syscache.h"
#include "utils/builtins.h"
#include "utils/lsyscache.h"
#include "utils/array.h"
#include "utils/typcache.h"

#include <SWI-Prolog.h>

#include <assert.h>

extern PL_extension plswipl_spi_extension[];

Datum plswipl_term_to_datum(term_t t, Oid type);
void plswipl_datum_to_term(Oid type, Datum datum, bool isnull, term_t t);
void plswipl_tuple_to_term(TupleDesc tupdesc, HeapTupleData *tuple, term_t a);
void plswipl_tuplename_to_term(TupleDesc tupdesc, term_t t);
