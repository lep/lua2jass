RUNTIME := runtime/Ins.j runtime/Interpreter.j runtime/Table.j runtime/Value.j  
RUNTIME += runtime/Context.j runtime/StringTable.j runtime/List.j runtime/Print.j
RUNTIME += runtime/Types.j runtime/Builtins.j runtime/Wrap.j runtime/Call.j
RUNTIME += runtime/Jass.j
RUNTIME += runtime/Builtin/Trigger.j runtime/Builtin/Player.j
RUNTIME += runtime/Builtin/Timer.j runtime/Builtin/Coroutine.j
AUTO	:= auto/Auto.j

OUT	:= $(patsubst runtime/%, out/%, $(RUNTIME))
OUT	+= $(patsubst auto/%, out/%, $(AUTO))

.PHONY: XXX
.PHONY: check

auto/Auto.j: test.lua
	runhaskell compile.hs test.lua --jass > auto/Auto.j

out/%.j: runtime/%.j
	perl process.pl $^ $@ lua_ 2>/dev/null

out/%.j: auto/%.j
	perl process.pl $^ $@ lua_ 2>/dev/null

check: war3map.j
	pjass $$commonj Blizzard.j war3map.j
	#pjass $$commonj Blizzard.j $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j

war3map.j: $(OUT) scaffold.j main.j
	#pjass $$commonj Blizzard.j $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j
	runhaskell jcat.hs $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j > $@


start-wc3: war3map.j
	rm -f ~/Library/Application\ Support/Blizzard/Warcraft\ III/CustomMapData/JHCR*.txt
	jhcr-start "$<"

update-wc3: war3map.j
	jhcr-update "$<"
