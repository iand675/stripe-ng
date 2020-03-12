{ sources ? import ./sources.nix }:
let
  nixpkgs = import sources.nixpkgs {};
  config = {};
  overlays = [
    (super: self: {
      inherit (import sources.niv {}) niv;
    })
  ];
in
  import sources.nixpkgs {
    inherit overlays config;
  }
