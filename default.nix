{ packages ? import ./pkgs.nix }:
let
  inherit (packages) pkgs;

  inputs = [
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-basic
        beamer
        etoolbox
        translator
        minted
        fvextra
        fancyvrb
        upquote
        lineno
        catchfile
        xstring
        framed
        float
        helvetic
        mathtools
        listings
        ulem
        booktabs;
    })
    pkgs.python38Packages.pygments
    pkgs.which
  ];
in
pkgs.stdenv.mkDerivation {
  name = "haskell-intro-type-safe-db-libs";
  buildInputs = inputs;
  src = builtins.fetchurl {
    url = "https://github.com/chiroptical/haskell-intro-type-safe-db-libs/archive/v0.1.tar.gz";
    sha256 = "0qm5xbqnwrr2ylpgpdmqnngnm4z089f2an5694ff5zb8m08dpwdq";
  };
  buildPhase = ''
    source $stdenv/setup
    mkdir -p $out
    pdflatex -shell-escape ./presentation.tex
    pdflatex -shell-escape ./presentation.tex
    pdflatex -shell-escape ./presentation.tex
  '';
  dontInstall = true;
}
