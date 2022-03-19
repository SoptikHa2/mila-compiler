DEPS=$(find . iname '*.hs')

.PHONY: compile clean run test
.DEFAULT_GOAL: compile

test: compile
	./test.sh

compile: $(DEPS)
	ghc MilaCompiler.hs

run: compile
	./Main

clean:
	rm -f $$(find . -iname '*.hi' -o -iname '*.o') MilaCompiler
