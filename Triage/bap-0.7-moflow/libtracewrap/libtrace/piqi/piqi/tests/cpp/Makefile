all: prep test


prep:
	piqi convert -t pb piqi.piqi


test:
	piqi to-proto piqi.piqi
	protoc --cpp_out=. piqi.piqi.proto
	$(MAKE) -f Makefile.cpp
	#protoc --decode=piqi_org.piqi.piqi piqi.piqi.proto < piqi.piqi.pb > t.ptext
	./ctest > t.ptext1
	#cmp t.ptext*


clean:
	rm -f piqi.piqi.* t.*
	$(MAKE) -f Makefile.cpp clean

