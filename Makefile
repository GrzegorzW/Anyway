PROJECT = anyway
PROJECT_DESCRIPTION = Yet another chat
PROJECT_VERSION = 0.1.0

DEPS = cowboy gun jiffy mochiweb
dep_cowboy_commit = master
dep_jiffy = git https://github.com/davisp/jiffy

DEP_PLUGINS = cowboy

include erlang.mk
