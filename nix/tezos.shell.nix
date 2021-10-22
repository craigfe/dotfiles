{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    hidapi
    autoconf
    pkgconfig
    coreutils
    libev
    libffi
    gmp
    zlib.dev
  ];
}
