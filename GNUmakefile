RUNTIME := runtime/Ins.j runtime/Interpreter.j runtime/Table.j runtime/Value.j
RUNTIME += runtime/Context.j runtime/StringTable.j runtime/List.j runtime/Print.j
RUNTIME += runtime/Types.j runtime/Builtins.j runtime/Wrap.j runtime/Call.j
RUNTIME += runtime/GC.j runtime/Deque.j runtime/Helper.j
RUNTIME += runtime/Builtin/Coroutine.j
RUNTIME += runtime/Builtin/Trigger.j
RUNTIME += runtime/Builtin/Timer.j
RUNTIME += runtime/Builtin/Boolexpr.j
RUNTIME += runtime/Builtin/Math.j
RUNTIME += runtime/Builtin/Table.j

AUTO := auto/Auto.j auto/Natives.j auto/Jass.j auto/Dispatch.j


OUT	:= $(patsubst runtime/%, out/%, $(RUNTIME))
OUT	+= $(patsubst auto/%, out/%, $(AUTO))

.PHONY: check clean

print-jorder:
	@echo $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j

runtime.dot: $(RUNTIME) $(AUTO)
	bash jdigraph.sh $(RUNTIME) $(AUTO) > "$@"

auto/Auto.j: test.lua
	runhaskell compile.hs test.lua --jass > auto/Auto.j

auto/Jass.j auto/Natives.j auto/Dispatch.j: common.j wrap-natives.hs
	runhaskell wrap-natives.hs common.j

out/Context.j: runtime/alloc.j runtime/alloc-globals.j
out/Interpreter.j: runtime/alloc.j runtime/alloc-globals.j
out/List.j: runtime/alloc.j runtime/alloc-globals.j
out/Table.j: runtime/alloc.j runtime/alloc-globals.j
out/Value.j: runtime/alloc.j runtime/alloc-globals.j

out/%.j: runtime/%.j
	perl process.pl $< $@ lua_ 2>/dev/null

out/%.j: auto/%.j
	perl process.pl $< $@ lua_ 2>/dev/null

check: war3map.j
	pjass $$commonj Blizzard.j war3map.j

war3map.j: $(OUT) scaffold.j main.j
	runhaskell jcat.hs $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j > $@


start-wc3: war3map.j
	rm -f ~/Library/Application\ Support/Blizzard/Warcraft\ III/CustomMapData/JHCR*.txt
	jhcr-start "$<"

update-wc3: war3map.j
	jhcr-update "$<"

clean:
	find out/ -type f -delete
	find auto/ -type f -delete
