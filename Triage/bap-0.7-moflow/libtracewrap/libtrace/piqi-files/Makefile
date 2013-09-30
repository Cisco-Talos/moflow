ROOTDIR=$(CURDIR)/..
TRACEFORMATDIR=$(ROOTDIR)/src/cpp/pb
MLDIR=$(ROOTDIR)/src/ocaml/piqi

# I don't think we need to compile all piqi files actually, only frame.piqi
PIQIFILE=frame.piqi
OTHERPIQIFILES=$(filter-out $(PIQIFILE), $(wildcard *.piqi))
PROTOFILES=$(addsuffix .proto, $(PIQIFILES))
MLFILES=$(addsuffix _piqi.ml, $(basename $(PIQIFILE)))
MLEXTFILES=$(addsuffix _piqi_ext.ml, $(basename $(PIQIFILE)))
PROTOCFILES=$(addsuffix .piqi.pb.cc, $(basename $(PIQIFILE)))
PROTOHFILES=$(addsuffix .piqi.pb.h, $(basename $(PIQIFILE)))

PIQIDIR=$(ROOTDIR)/piqi/piqi/bin
PIQI=$(PIQIDIR)/piqi
PIQIC=$(PIQIDIR)/piqic
PROTOC=$(ROOTDIR)/protobuf/src/protoc

PIQIFLAGS = --pp
#PIQIFLAGS = --pp --leave-tmp-files

all: $(PROTOCFILES) $(PROTOHFILES) $(MLFILES) $(MLEXTFILES)

$(PIQIFILE) : $(OTHERPIQIFILES)
	touch $@

%.piqi.proto : %.piqi
	$(PIQI) to-proto $<

%.piqi.pb.cc %.piqi.pb.hh : %.piqi.proto
	$(PROTOC) $< --cpp_out=.

#%_piqi.ml : %.piqi
#	piqic ocaml $(PIQIFLAGS) $<

%_piqi.ml %_piqi_ext.ml : %.piqi
	$(PIQIC) ocaml-ext $(PIQIFLAGS) $<

clean:
	rm -f $(PROTOFILES) $(MLFILES) $(MLEXTFILES) $(PROTOCFILES) $(PROTOHFILES)

.PHONY: install-pb install-piqi install
install-pb: $(PROTOCFILES) $(PROTOHFILES)
	cp $^ $(TRACEFORMATDIR)

install-piqi: $(MLFILES) $(MLEXTFILES)
	cp $^ $(MLDIR)

install: install-pb install-piqi
