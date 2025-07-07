{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    openxc7 = {
      url = "github:openxc7/toolchain-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, openxc7 }: {

    packages = openxc7.packages;

    devShell = openxc7.devShell;

  };
}
