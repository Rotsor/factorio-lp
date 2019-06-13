{ stdenv, fetchFromGitHub, ocaml, findlib, dune, glpk }:

let __dune = dune; in
let dune = __dune.override { ocamlPackages = { inherit ocaml findlib; }; };
in

stdenv.mkDerivation rec {
  name = "ocaml${ocaml.version}-glpk-${version}";
  version = "20180726";
  src = fetchFromGitHub {
    owner = "smimram";
    repo = "ocaml-glpk";
    rev = "8c7817dd7a7d2d9af7299c48a875aa4eea5ae086";
    sha256 = "00ikqx96i9n6qz952q7mmpzwfcgw8ap4g5c68iz8yjwlj2wix4ij";
  };

  patches = [
  ];

  buildInputs = [ ocaml findlib dune glpk ];

  propagatedBuildInputs = [ glpk ];

  configureFlags = [ 
     "--prefix=$out"
     ];

  buildPhase =
    "cd src; echo  \"$OCAMLFIND_DESTDIR\"; cat Makefile; mkdir -p \"$OCAMLFIND_DESTDIR\"; make opt; ls $out";

  installTargets = "install";

  meta = with stdenv.lib; {
    description = "GLPK bindings for OCaml";
    license     = licenses.gpl2;
    maintainers = with maintainers; [ aalekseyev ];
    inherit (src.meta) homepage;
    inherit (ocaml.meta) platforms;
  };
}
