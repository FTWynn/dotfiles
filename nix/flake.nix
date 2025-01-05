{
  description = "FTWynn Dev Machine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
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
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              #inherit pkgs;
              #extraSpecialArgs = {
              #  inherit pkgs-unstable;
              #};
              home-manager.users.david = import ./home.nix;
              home-manager.extraSpecialArgs = {
                inherit pkgs-unstable;
              };
              # Optionally, use home-manager.extraSpecialArgs to pass arguments to home.nix
            }
          ];
        };
        david-framework = lib.nixosSystem {
          specialArgs = {
            inherit inputs;
            inherit pkgs-unstable;
          };
          inherit system;
          modules = [
            /etc/nixos/configuration.nix
            # inputs.home-manager.nixosModules.default
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              #inherit pkgs;
              #extraSpecialArgs = {
              #  inherit pkgs-unstable;
              #};
              home-manager.users.david = import ./home.nix;
              home-manager.extraSpecialArgs = {
                inherit pkgs-unstable;
              };
              # Optionally, use home-manager.extraSpecialArgs to pass arguments to home.nix

              # Some stuff to make flakes work at work
              # source: https://nixos-and-flakes.thiscute.world/best-practices/nix-path-and-flake-registry#custom-nix-path-and-flake-registry-1
              nix.registry.nixpkgs.flake = nixpkgs;
              environment.etc."nix/inputs/nixpkgs".source = "${nixpkgs}";
              nix.settings.nix-path = lib.mkForce "nixpkgs=/etc/nix/inputs/nixpkgs";
            }
          ];
        };
      };
#      homeConfigurations = {
#        david = home-manager.lib.homeManagerConfiguration {
#          inherit pkgs;
#          extraSpecialArgs = {
#            inherit pkgs-unstable;
#          };
#          modules = [ ./home.nix ];
#        };
#      };
    };
}
