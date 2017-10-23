# Impure: use the host nixpkgs:
# This has been tested with: 
{ pkgs ? import <nixpkgs> {}
#, stdenv
#, ocaml
}:

with pkgs;

stdenv.mkDerivation rec {
  name = "dirsize-${version}";
  version = "0.9";

  buildInputs = [ ocaml ];

  src = builtins.filterSource
    (path: type: type != "directory" ||
    (baseNameOf path != ".git" && baseNameOf path != "haskell_ver"))
      ./. ;

  # This is hacky because the Makefile builds in-place (mutates src dir):
  builder = builtins.toFile "builder.sh" "
    source $stdenv/setup
    cp -a $src/ ./src
    chown -R `whoami` ./src
    chmod u+rwX -R ./src
    cd ./src
    make
    mkdir -p $out/bin
    cp dirsize mods $out/bin/
  ";

}
