RUNTIME := runtime/Ins.j runtime/Interpreter.j runtime/Table.j runtime/Value.j
RUNTIME += runtime/Context.j runtime/StringTable.j runtime/List.j runtime/Print.j
RUNTIME += runtime/Types.j runtime/Builtins.j runtime/Wrap.j runtime/Call.j
RUNTIME += runtime/GC.j runtime/Deque.j runtime/Helper.j runtime/Main.j
RUNTIME += runtime/Builtin/Coroutine.j
RUNTIME += runtime/Builtin/Trigger.j
RUNTIME += runtime/Builtin/Timer.j
RUNTIME += runtime/Builtin/Boolexpr.j
RUNTIME += runtime/Builtin/Math.j
RUNTIME += runtime/Builtin/Table.j

.PHONY: check clean
.PHONY: start-wc3 update-wc3

check: war3map.j
	pjass $$commonj Blizzard.j war3map.j

clean:
	rm -f war3map.j lua2jass.j

lua2jass.j: test.lua $(RUNTIME)
	runhaskell Main.hs -- $$commonj $< $@

war3map.j: lua2jass.j scaffold.j main.j
	cat $^ > $@

start-wc3: war3map.j
	rm -f ~/Library/Application\ Support/Blizzard/Warcraft\ III/CustomMapData/JHCR*.txt
	jhcr-start "$<"

update-wc3: war3map.j
	jhcr-update "$<"

