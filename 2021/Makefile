all: day01.out day02.out day03.out day04.out day05.out day06.out day07.out day08.out day09.out day10.out day11.out day12.out day13.out day14.out day15.out day16.out day17.out day18.out day19.out day20.out day21.out day22.out day23.out day24.out day25.out

day7.out: day7.ml
	ocamlopt -o $@ str.cmxa $<
	./day7.out

%.out: %.ml
	ocamlopt -o $@ $<
	./$@

clean:
	rm -fr *.o *.cm[iox] *.out



