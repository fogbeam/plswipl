MODULE_big = plswipl
EXTENSION = plswipl
DOCS = README.plswipl
OBJS = plswipl.o spi.o

plswipl_version = 0.1

DATA = boot.prolog
DATA_built = plswipl--$(plswipl_version).sql plswipl--unpacked--$(plswipl_version).sql

PKG_CONFIG = pkg-config
PG_CPPFLAGS += $(shell $(PKG_CONFIG) --cflags-only-I swipl) -DPLSWIPL_VERSION='"$(plswipl_version)"'
SHLIB_LINK  += $(shell $(PKG_CONFIG) --libs          swipl)

MODULEDIR=plswipl

PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)

include $(PGXS)

plswipl--$(plswipl_version).sql: plswipl.sql
	cat $< >$@

plswipl--unpacked--$(plswipl_version).sql: plswipl--unpacked.sql
	cat $< >$@
