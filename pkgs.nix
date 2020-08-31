let
  pkgs =
    import
      (
        builtins.fetchTarball {
          name = "nixos-20.03";
          url = "https://github.com/NixOS/nixpkgs/archive/925ae0dee63cf2c59533a6258340812e5643428a.tar.gz";
          sha256 = "1g3kkwyma23lkszdvgb4dn91g35b082k55ys8azc7j4s6vxpzmaw";
        }
      ) { };
in
{
  pkgs = pkgs;
}
