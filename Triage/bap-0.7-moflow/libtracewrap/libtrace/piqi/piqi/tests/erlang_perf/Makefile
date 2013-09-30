.PHONY: all erlang test clean


all: erlang test


erlang: piqi_obj.piqi
	$(MAKE) -f Makefile.erlang


piqi_obj.piqi: ../../piqi/piqi.piqi
	piqi expand $< >$@


test: erlang
	piqi convert -t pb addressbook.piq

	echo ":piqi_obj/piqi [" > piqi.piq
	cat piqi_obj.piqi >> piqi.piq
	echo "]" >> piqi.piq

	piqi convert --add-defaults -t pb --no-warnings piqi.piq
	#./test


clean:
	$(MAKE) -f Makefile.erlang clean
	rm -f addressbook.piq.pb piqi.* piqi_obj.*

