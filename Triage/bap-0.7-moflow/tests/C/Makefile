include Makefile.defs

CFLAGS = -m32 -fno-stack-protector

all: $(CEXECS)

$(CEXECS): $(CSRC)
	gcc $@.c -o $@ $(CFLAGS)

clean:
	rm -f $(CEXECS)
