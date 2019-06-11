.PHONY: test build

DOTS=$(sort $(wildcard _build/default/test/*.dot))

test:
	dune build @check
	if dune runtest --no-buffer; then make svgs; else make svgs; exit 1; fi

svg:
	mkdir svg

svg/%.svg: svg _build/default/test/%.dot
	@dot -Tsvg $^ -o $@

# Create svg/* files from the test result dot files.
svgs: $(addprefix svg/,$(addsuffix .svg,$(basename $(notdir ${DOTS}))))

build:
	dune build @all

clean:
	dune clean
	rm -f svg/*.svg
