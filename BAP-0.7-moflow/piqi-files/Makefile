ROOTDIR=$(CURDIR)/..
TRACEDIR=$(ROOTDIR)/libtracewrap/libtrace
PBDIR=$(CURDIR)/protobuf
MLDIR=$(ROOTDIR)/ocaml/piqi

# I don't think we need to compile all piqi files actually, only frame.piqi
PIQIFILE=stmt.piqi
OTHERPIQIFILES=$(filter-out $(PIQIFILE), $(wildcard *.piqi))
PROTOFILES=$(addsuffix .proto, $(PIQIFILES))
MLFILES=$(addsuffix _piqi.ml, $(basename $(PIQIFILE)))
MLEXTFILES=$(addsuffix _piqi_ext.ml, $(basename $(PIQIFILE)))
PROTOCFILES=$(addprefix $(PBDIR)/, $(addsuffix .piqi.pb.cc, $(basename $(PIQIFILE))))
PROTOHFILES=$(addprefix $(PBDIR)/, $(addsuffix .piqi.pb.h, $(basename $(PIQIFILE))))

PIQIDIR=$(TRACEDIR)/piqi/piqi/bin
PIQI=$(PIQIDIR)/piqi
PIQIC=$(PIQIDIR)/piqic
PROTOC=$(TRACEDIR)/protobuf/src/protoc

#PIQIFLAGS = --pp
#PIQIFLAGS = --pp --leave-tmp-files

all: $(PROTOCFILES) $(PROTOHFILES) $(MLFILES) $(MLEXTFILES)

$(PIQIFILE) : $(OTHERPIQIFILES)
	touch $@

%.piqi.proto : %.piqi
	$(PIQI) to-proto $<

$(PBDIR)/%.piqi.pb.cc $(PBDIR)/%.piqi.pb.hh : %.piqi.proto
	$(PROTOC) $< --cpp_out=$(PBDIR)
	$(PROTOC) $< --java_out=$(PBDIR)
	$(PROTOC) $< --python_out=$(PBDIR)

#%_piqi.ml : %.piqi
#	piqic ocaml $(PIQIFLAGS) $<

%_piqi.ml %_piqi_ext.ml : %.piqi
	$(PIQIC) ocaml-ext $(PIQIFLAGS) $<

clean:
	rm -f $(PROTOFILES) $(MLFILES) $(MLEXTFILES) $(PROTOCFILES) $(PROTOHFILES)

.PHONY: install-piqi install
# install-pb: $(PROTOCFILES) $(PROTOHFILES)
# 	cp $^ $(PBDIR)

install-piqi: $(MLFILES) $(MLEXTFILES)
	cp $^ $(MLDIR)

install: install-piqi
