{
  description = "FTWynn Dev Machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      commonArgs = { inherit system; config.allowUnfree = true; };
      pkgs = import nixpkgs commonArgs;
      pkgs-unstable = import nixpkgs-unstable commonArgs;
    in
    {
      nixosConfigurations = {
        nixos-thinkpad = lib.nixosSystem {
          specialArgs = {
            inherit inputs;
            inherit pkgs-unstable;
          };
          inherit system;
          modules = [
            ./configuration.nix
            # inputs.home-manager.nixosModules.default
          ];
        };
      };
      homeConfigurations = {
        david = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit pkgs-unstable;
          };
          modules = [ ./home.nix ];
        };
      };
    };
}
