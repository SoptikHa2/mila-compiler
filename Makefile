DEPS=$(find . iname '*.hs')

.PHONY: compile clean run
.DEFAULT_GOAL: compile

compile: $(DEPS)
	ghc Main.hs

run: compile
	./Main

clean:
	rm -f $$(find . -iname '*.hi' -o -iname '*.o') Main
