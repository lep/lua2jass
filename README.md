# lua2jass 

## How to use

Provide a common.j and a lua file.

```
$ lua2jass common.j war3map.lua war3map.j
```

This will then produce the file war3map.j. Do note that this script file is not complete yet.
You need to provide your own `config` and `main` functions (in jass of course). And you need to
call the function `lua_Main_init` (`call ExecuteFunc("lua_Main_init")`).

## What doesn't work (yet, or maybe never)

- Labels and goto 
- Not every argument to [collectcarbage](https://www.lua.org/manual/5.3/manual.html#pdf-collectgarbage).
- No method from [string](https://www.lua.org/manual/5.3/manual.html#6.4)
- [tonumber](https://www.lua.org/manual/5.3/manual.html#pdf-tonumber)
- A bunch of natives (see Main.hs)

## How to build

Check out this repository and do `cabal build --allow-newer`.
Or if you run nix you can do `nix build github:lep/lua2jass`.
