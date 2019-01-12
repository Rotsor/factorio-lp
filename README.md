# factorio-lp
factorio optimization: linear programming approach

To compile:

- Have a unix-like environment (most linuxes should work, [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10) works)
- Install [opam](https://opam.ocaml.org/doc/Install.html)
- install the necessary libraries with opam (see jbuild for the list of libraries)
- Use `dune build main.exe` to build
- Find the binary in `_build` directory
