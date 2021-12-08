%: day%.out
	./$^

day%.out: day%.ml
	ocamlopt -o $@ $^

clean:
	rm -fr *.o *.cm[iox] *.out
