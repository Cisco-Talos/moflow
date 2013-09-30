
RESULT = proto2piqi
TEST = descriptor.piqi

CXXFLAGS += -I.


all: $(RESULT) test


$(RESULT): proto2piqi.cc main.cc
	$(CXX) $(CXXFLAGS) -o $@ $^ -lprotobuf -lprotoc -lpthread


.PHONY: clean test


clean:
	rm -f *.o $(RESULT)
	rm -f *.pb*
	rm -f *.piqi*


test: $(TEST)


$(TEST): descriptor.proto $(RESULT) 
	./proto2piqi $< | sed -e 's/_/-/g' > $@.tmp.piqi
	piqi check $@.tmp.piqi
	piqi pp -o $@ --normalize-words $@.tmp.piqi


# protoc -odescriptor.pb google/protobuf/descriptor.proto

