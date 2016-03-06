#include "postgres.h"
#include "fmgr.h"
#include "utils/guc.h"
#include "commands/trigger.h"
#include "miscadmin.h"
#include "port.h"

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

Datum
plswipl_inline(PG_FUNCTION_ARGS) {
    InlineCodeBlock *codeblock = (InlineCodeBlock *) PG_GETARG_POINTER(0);
    term_t a0 = PL_new_term_refs(1);
    static predicate_t p;
    if (!p)
        p = PL_predicate("do", 1, "plswipl_low");
    PL_put_string_chars(a0, codeblock->source_text);
    PL_call_predicate(NULL, PL_Q_CATCH_EXCEPTION, p, a0);
    /* PL_call_predicate(NULL, PL_Q_NORMAL, p, a0); */
    PG_RETURN_VOID();
}

Datum
plswipl_validator(PG_FUNCTION_ARGS) {
    PG_RETURN_VOID();
}

Datum
plswipl_function(PG_FUNCTION_ARGS) {
    printf("flinfo: %p, context: %p, resultinfo: %p, oid: %i, isnull: %i, nargs: %i\n",
           cfinfo->flinfo,
           cfinfo->context,
           cfinfo->resultinfo,
           cfinfo->fncollation,
           cfinfo->isnull,
           cfinfo->nargs);

    
    PG_RETURN_VOID();
}
