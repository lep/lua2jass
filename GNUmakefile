RUNTIME := runtime/Ins.j runtime/Interpreter.j runtime/Table.j runtime/Value.j  
RUNTIME += runtime/Context.j runtime/StringTable.j runtime/List.j runtime/Print.j
RUNTIME += runtime/Types.j runtime/Builtins.j
AUTO	:= auto/Auto.j

OUT	:= $(patsubst runtime/%, out/%, $(RUNTIME))
OUT	+= $(patsubst auto/%, out/%, $(AUTO))

.PHONY: XXX
.PHONY: check

auto/Auto.j: test.lua
	runhaskell compile.hs test.lua --jass > auto/Auto.j

out/%.j: runtime/%.j
	bash process.sh $^ $@ lua_ 2>/dev/null

out/%.j: auto/%.j
	bash process.sh $^ $@ lua_ 2>/dev/null

check: $(OUT)
	pjass $$commonj Blizzard.j $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j

war3map.j: $(OUT) scaffold.j main.j
	pjass $$commonj Blizzard.j $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j
	runhaskell jcat.hs $$(./jorder.sh $(RUNTIME) $(AUTO)) scaffold.j main.j > $@


start-wc3: war3map.j
	jhcr-start "$<"

update-wc3: war3map.j
	jhcr-update "$<"
