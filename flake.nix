{
  inputs = {
    bellroy-nix-foss = {
      url = "github:bellroy/bellroy-nix-foss";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      cabalPackages = [
        {
          name = "servant-activeresource";
          path = ./servant-activeresource.nix;
        }
      ];
      supportedCompilers = [ "ghc8107" "ghc92" "ghc94" ];
      defaultCompiler = "ghc92";
#      haskellPackagesOverride = { compilerName, haskellLib, final, prev }:
#        if compilerName == "ghc94"
#        then {
#          # hal doesn't support newer hedgehog
#          hedgehog = haskellLib.compose.dontCheck (prev.callHackage "hedgehog" "1.1.2" { });
#        }
#        else { };
    };
}
