ALTER EXTENSION plswipl ADD PROCEDURAL LANGUAGE plswipl;

ALTER EXTENSION plswipl ADD FUNCTION plswipl_call_handler();
ALTER EXTENSION plswipl ADD FUNCTION plswipl_inline_handler(internal);
ALTER EXTENSION plswipl ADD FUNCTION plswipl_validator(oid);
