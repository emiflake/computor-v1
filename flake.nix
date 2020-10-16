{
  description = "computor-v1 equation solver";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
  inputs.haskell.url = "github:input-output-hk/haskell.nix";

  outputs = { self, nixpkgs, haskell }:
    let
      supportedSystems =
        [ "x86_64-linux" "i686-linux" "aarch64-linux" "x86_64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in {

      packages = forAllSystems (system:
        let
          pkgs = haskell.legacyPackages."${system}";
          drv = (pkgs.haskell-nix.project {
            src = pkgs.haskell-nix.haskellLib.cleanGit {
              name = "computor-v1";
              src = ./.;
            };
          });
        in { computor-v1 = drv.computor-v1.components.exes.computor-v1-exe; });

      defaultPackage =
        forAllSystems (system: self.packages."${system}".computor-v1);

      defaultApp = forAllSystems (system: {
        type = "app";
        program = "${self.defaultPackage."${system}"}/bin/computor-v1-exe";
      });
    };
}
