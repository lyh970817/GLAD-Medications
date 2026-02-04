# shell.nix
{ pkgs ? import <nixpkgs> {} }:

let
  # Your pinned source (e.g. for R 4.2.2)
  pinnedPkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8ad5e8132c5dcf977e308e7bf5517cc6cc0bf7d8.tar.gz") {};
  
  # The R interpreter from that time
  myR = pinnedPkgs.R; 

in pkgs.mkShell {
  name = "r-project-env";

  nativeBuildInputs = with pkgs; [
    pkg-config
    gnumake
    gcc
    gfortran
  ];

  # CORRECTED buildInputs
  buildInputs = [ 
    # 1. The Pinned R
    myR 
    
    # 2. The Language Server FROM THE SAME PINNED SOURCE
    #    (Do NOT use pkgs.rPackages here)
    pinnedPkgs.rPackages.languageserver 
  
  ] ++ (with pinnedPkgs.pkgs; [
    # 3. System libraries can usually stay from 'pkgs' (current system)
    #    unless they have strict version ties to R (rare for these libs)
    libtiff
    openssl
    curl
    libxml2
    freetype
    v8
    harfbuzz
    fribidi
    fontconfig
    libpng
    libjpeg
    # ... other system libs ...
  ]);

  shellHook = ''
# Calculate the path for the Makevars file
    mkdir -p .R
    
    # We bake the library path into the compiled packages using -rpath
    echo "LDFLAGS += -Wl,-rpath,${pkgs.lib.makeLibraryPath (with pinnedPkgs; [
      openssl curl libxml2 libtiff libsodium freetype
    ])}" > .R/Makevars
    
    # Tell R to use our custom build config
    export R_MAKEVARS_USER=$(pwd)/.R/Makevars
  '';

}
