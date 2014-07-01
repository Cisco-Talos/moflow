# $Id: Makefile 5785 2012-02-17 19:58:05Z edmcman $
# Piqi wrapper Makefile

.PHONY: all clean tags

all:	deps
# Annoying, but we need bash for this because Piqi people don't know
# how to make a Makefile.
	bash -c 'cd piqi && source setenv.sh && $(MAKE)'

deps:
	echo Building deps
	bash -c 'cd piqi && source setenv.sh && $(MAKE) deps'
	touch deps

clean:
	$(RM) deps
	bash -c 'cd piqi && source setenv.sh && $(MAKE) distclean'

tags:

