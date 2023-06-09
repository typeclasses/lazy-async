{
  inputs = {
    "nixos-23.05".url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "lazy-async";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.05" = import inputs."nixos-23.05" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.05";
        project = pkgs.haskellPackages.developPackage {
          root = ./lazy-async;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in
      {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations =
            let

              inherit (pkgs.haskell.lib) dontCheck;

              makeTestConfiguration =
                let defaultPkgs = pkgs;
                in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
                  let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
                  in (pkgs.haskell.packages.${ghcVersion}.override (old: {
                    overrides = combineOverrides old [
                      (packageSourceOverrides {
                        lazy-async = ./lazy-async;
                      })
                      overrides
                    ];

                  })).lazy-async;

            in
            rec {
              ghc-9-0 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc90";
              };
              ghc-9-2 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc92";
              };
              ghc-9-4 = makeTestConfiguration {
                pkgs = nixpkgs."nixos-23.05";
                ghcVersion = "ghc94";
              };
              all = pkgs.symlinkJoin {
                name = packageName;
                paths = [ ghc-9-0 ghc-9-2 ghc-9-4 ];
              };
            };
        };
      });
}
