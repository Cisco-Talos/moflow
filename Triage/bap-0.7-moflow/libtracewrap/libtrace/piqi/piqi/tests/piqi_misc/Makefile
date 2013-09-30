

PIQI_FILES = \
	piqast.piqi \
	piqobj.piqi \
	piqtype-core.piqi


test: test_piqi test_piqtype_core


test_piqi:
	set -e; \
	for i in $(PIQI_FILES:%.piqi=%); \
	do \
		../examples/test_piqi $$i --no-warnings >/dev/null; \
	done


# test that piqtype-core.piqi can parse itself
test_piqtype_core:
	cat piqtype-core.piqi | sed -e 's!\.module.*!(:piqtype-core/typedef)!;s!.piq-format.word!!' >piqtype-core.piq
	piqi check piqtype-core.piq


clean:
	rm -f p.piqi pp.piqi p.piqi.text *.proto *.pib *.pb *.json *.xml piqtype-core.piq

