# this is based on coq's default.nix
# if some things make no sense here, they might have been inherited

# nix-shell supports the --arg option (see Nix doc) that allows you for
# instance to do this:
# $ nix-shell --arg pkgs "(import <nixpkgs> {})"

{ pkgs ?
    (import

(fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/4d2a989b7c235964e5b6de202aec193581f873f9.tar.gz";
      sha256 = "0jfn85hnp20b7rqghajad9wv2m0sz8f4bfspz340ifrlh1xk7l09";
    })
    {})
, ocamlPackages ? pkgs.ocaml-ng.ocamlPackages_4_07
, shell ? false
}:

with pkgs;
with stdenv.lib;

let ocaml_glpkq = ocamlPackages : with ocamlPackages;
 callPackage ./glpk.nix { glpk = pkgs.glpk; };
in

let add_glpk = self: super: { ocaml_glpk = ocaml_glpkq self; } // super; in

let p = ocamlPackages; in
let ocamlPackages = p.overrideScope' add_glpk; in

stdenv.mkDerivation rec {

  name = "factorio-lp";

  buildInputs = [
    dune
  ]
  ++ (with ocamlPackages; [
     ocaml
     findlib
     bignum
     ppx_jane
     core_kernel
     ocamlPackages.async
     async_interactive
     ocaml_glpk
     ])
  ++ optionals shell (
    (with ocamlPackages; [ merlin ocp-indent ocp-index utop ])
  );

  src =
    if shell then null
    else
      with builtins; filterSource
        (path: _:
           !elem (baseNameOf path) [".git" "result" "bin" "_build" "_build_ci"]) ./.;

  preConfigure = ''
  '';

  prefixKey = "-prefix ";

  buildFlags = [ ];

  createFindlibDestdir = !shell;

  preInstallCheck = ''
  '';

  installCheckTarget = [ "check" ];

  passthru = {
    inherit ocamlPackages;
    dontFilter = true; # Useful to use mkCoqPackages from <nixpkgs>
  };

  setupHook = writeText "setupHook.sh" "
  ";

  meta = {
    description = "factorio lp solver";
    longDescription = ''
      This includes:
      - a lua script to export the relevant recipe data from factorio
      - an OCaml program that can compute shadow prices for various goods
      based on available recipes and/or designs optimal factory given a
      set of constraints.
    '';
    homepage = http://coq.inria.fr;
    license = licenses.unlicense;
    platforms = platforms.unix;
  };

}
