# shell.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "r-project-env";

  nativeBuildInputs = with pkgs; [
    pkg-config
    gnumake
    gcc
    gfortran
    cmake
  ];

  # CORRECTED buildInputs
  buildInputs = [
    # 1. The R from current nixpkgs
    pkgs.R

    # 2. The Language Server
    pkgs.rPackages.languageserver

  ] ++ (with pkgs; [
    # 3. System libraries
    libtiff
    openssl
    curl
    libxml2
    freetype
    harfbuzz
    fribidi
    fontconfig
    libpng
    libjpeg
    libsodium
    libwebp
    nlopt
  ]);

  shellHook = ''
# Calculate the path for the Makevars file
    mkdir -p .R

    # We bake the library path into the compiled packages using -rpath
    echo "LDFLAGS += -Wl,-rpath,${pkgs.lib.makeLibraryPath (with pkgs; [
      openssl curl libxml2 libtiff libsodium freetype
      harfbuzz fribidi fontconfig libpng libjpeg libwebp
    ])}" > .R/Makevars

    # Tell R to use our custom build config
    export R_MAKEVARS_USER=$(pwd)/.R/Makevars
  '';

}
