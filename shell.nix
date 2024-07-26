with (import <nixpkgs> {config.allowUnfree = true;});

let 
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      zlib
      haskell.compiler.ghc910
      cabal-install
    ];

    shellHook = ''
      PATH=~/.cabal/bin:$PATH
      LD_LIBRARY_PATH=${pkgs.zlib}/lib/:$LD_LIBRARY_PATH
    '';
  }
