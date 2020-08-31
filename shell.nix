{ packages ? import ./pkgs.nix }:
let
  inherit (packages) pkgs;
in
pkgs.mkShell {
  buildInputs =
    [
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
    ];
}
