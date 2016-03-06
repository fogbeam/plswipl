/*CREATE FUNCTION plswipl_call_handler();
CREATE FUNCTION plswipl_validator_handler(oid);
*/
/*CREATE PROCEDURAL LANGUAGE plswipl
       HANDLER plswipl_call_handler
       VALIDATOR plswipl_validator_handler;

COMMENT ON PROCEDURAL LANGUAGE plswipl IS 'PL/SWI-Prolog procedural language';
*/



CREATE FUNCTION plswipl_handler()
RETURNS language_handler
AS 'MODULE_PATHNAME'
LANGUAGE C;

CREATE FUNCTION plswipl_inline(internal)
RETURNS void
AS 'MODULE_PATHNAME'
LANGUAGE C;

CREATE FUNCTION plswipl_validator(oid)
RETURNS void
AS 'MODULE_PATHNAME'
LANGUAGE C;

CREATE PROCEDURAL LANGUAGE plswipl HANDLER plswipl_handler INLINE plswipl_inline VALIDATOR plswipl_validator;


