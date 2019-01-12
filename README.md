# factorio-lp
factorio optimization: linear programming approach

To compile:

# 1. Get POSIX-ish environment

Most linuxes should work, [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) works.

# 2. Prepare a build environment.

There are two ways to do that:

## a) OPAM

- Install [opam](https://opam.ocaml.org/doc/Install.html)
- install the necessary libraries with opam (see [jbuild](jbuild) for the list of libraries)
- `opam install dune`

## b) Nix

- Install [nix](https://nixos.org/nix/)
- Run `nix-shell`

# 3. Use

To export recipe data, use [script.lua] (paste the entire contents of the file into
factorio console (yes, multiline commands are fine)).

That generates a file generated.ml in ~/.factorio/script-output/
which you need to copy over generated.ml in this repo and re-compile.

Then run:

```
dune build main.exe
./_build/default/main.exe improve-hardcoded
```

and see some nonsense that you can't interpret easily.