{
  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-unstable-small"; };
  };

  outputs = inputs:
    let
      nixpkgs = inputs.nixpkgs;
      supportedSystems = [ "x86_64-linux" ];
    in
    {
      packages = nixpkgs.lib.genAttrs supportedSystems (system: let
        haskellPackages = inputs.nixpkgs.legacyPackages."${system}".haskell.packages.ghc946;
      in {
        default = haskellPackages.callCabal2nix "nvidia-dev" ./. {};
      });
    };
}
