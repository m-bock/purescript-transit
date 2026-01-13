{
  description = "PureScript Transit development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
            nodePackages.vega-lite
            nodePackages.vega-cli
            pandoc
            graphviz
            just
            noto-fonts-color-emoji
            noto-fonts
            librsvg
            (pkgs.texlive.withPackages (ps: with ps; [
              scheme-small
              xetex
              newunicodechar
              fontspec
              svg
              transparent
              framed
            ]))
          ];
        };
      });
}

