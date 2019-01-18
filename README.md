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

That generates a file `game-data.sexp` in `script-output` directory in factorio.
If that happens to be in `~/.factorio/script-output/game-data.sexp` then factorio-lp
will find it automatically. If not, you'll need to point it to it.

Then modify the program to make it compute what you want (which might involve rewriting much of it) and run:

```
dune build main.exe
./_build/default/main.exe solve
```