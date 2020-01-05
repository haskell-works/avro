let 
  vscode-overlay = self: super: {
    vscode-with-extensions = super.vscode-with-extensions.override {
      vscodeExtensions = with super.vscode-extensions; [
        bbenoist.Nix
      ] ++ super.vscode-utils.extensionsFromVscodeMarketplace [
        {
            name = "language-haskell";
            publisher = "justusadam";
            version = "2.6.0";
            sha256 = "1891pg4x5qkh151pylvn93c4plqw6vgasa4g40jbma5xzq8pygr4";
        }
        {
            name = "vscode-hie-server";
            publisher = "alanz";
            version = "0.0.32";
            sha256 = "1giihg8zs5fb3cwkmibdgpn7q040bhzh3a3jw1brmmb9ba15gljn";
        }
      ];
    };
  };
in
  with import <unstable> {
    overlays = [ vscode-overlay ];
  };

  pkgs.mkShell {
    buildInputs = with pkgs; [
      zlib
      ghc
      cabal-install
      vscode-with-extensions
    ];

    shellHook = ''
      PATH=~/.cabal/bin:$PATH
      LD_LIBRARY_PATH=${pkgs.zlib}/lib/:$LD_LIBRARY_PATH
    '';
  }
