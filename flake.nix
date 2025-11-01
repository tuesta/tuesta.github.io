{
  description = "tuesta.me";

  nixConfig.bash-prompt = "[nix]λ. ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "tuesta-me";

        # this approach is more common
        overlay = self: super: {
          # another name to still have access to the non-overlay version
          myHaskellPackages = super.haskell.packages.ghc9103.override (old: {
            # take from this | https://github.com/NixOS/nixpkgs/issues/26561#issuecomment-397350884
            # it's very weird
            overrides =  self.lib.composeExtensions (old.overrides or (_:_: {}))
              (hself: hsuper: {
                ${packageName} = hself.callCabal2nix
                  packageName
                  # this is necessary?, or just ./.
                  (builtins.path { path = ./.; name = packageName; })
                  { };
              });
          });
        };
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };

        # the reason for not use .extend | https://github.com/NixOS/nixpkgs/issues/26561#issuecomment-397350884
        # pkgs = nixpkgs.legacyPackages.${system};
        # haskellPackages = pkgs.haskell.packages.ghc923.extend (hself: hsuper: {
        #   intervals = hself.callCabal2nix packageName ./. { };
        # });
      in {

        # this is correct? .callCabal2nix two times?
        #   packages.${packageName} = pkgs.haskellPackages.callCabal2nix packageName self { };
        # I'm using this because it seems more correct
        packages.${packageName} = pkgs.myHaskellPackages.${packageName};
        packages.default = self.packages.${system}.${packageName};

        apps.${packageName} = flake-utils.lib.mkApp { drv = self.packages.${system}.${packageName}; };
        apps.default = self.apps.${system}.${packageName};

        devShell = pkgs.myHaskellPackages.shellFor {
          # what is the p argument?
          packages = p: [ p.${packageName} ];
          # i'm using haskellPackages, no myHaskellPackages
          buildInputs = with pkgs.haskellPackages; [ cabal-install pkgs.nodePackages.serve ];
          withHoogle = false;
        };

      });
}
