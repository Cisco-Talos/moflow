# 
# 

TESTS = test_list.ml test_list2.ml test_stack.ml

test: test_suite example
	-./test_suite
	-./example

test_suite: $(TESTS) test_suite.ml
	ocamlfind ocamlc -o test_suite -package oUnit -linkpkg \
	test_list.ml test_list2.ml test_stack.ml test_suite.ml	

example: example.ml
	ocamlfind ocamlc -o example -package oUnit -linkpkg \
	example.ml

clean:
	-$(RM) *.cmi *.cmo test_suite example
