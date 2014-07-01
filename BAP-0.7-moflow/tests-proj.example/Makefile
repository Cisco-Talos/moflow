export BAP_PROJ_OCAMLDIR=$(BAP_DIR)/../ocaml-proj
export BAP_PROJ_UTILDIR=$(BAP_DIR)/../utils-proj
export BAP_PROJ_TESTDIR=$(BAP_DIR)/../tests-proj

define PROJ_testsuite
	RESULT=project_test_suite
	SOURCES = $(BAP_PROJ_TESTDIR)/project_suite.ml
	DOC_FILES=$(SOURCES)
endef
export PROJ_testsuite

export PROJTESTS = testsuite
