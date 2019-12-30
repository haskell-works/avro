let 
  vscode-overlay = self: super: {
    vscode-with-extensions = super.vscode-with-extensions.override {
      vscodeExtensions = with super.vscode-extensions; [
        bbenoist.Nix
      ] ++ super.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "ghcide";
          publisher = "DigitalAssetHoldingsLLC";
          version = "0.0.2";
          sha256 = "02gla0g11qcgd6sjvkiazzk3fq104b38skqrs6hvxcv2fzvm9zwf";
        }
        {
            name = "language-haskell";
            publisher = "justusadam";
            version = "2.6.0";
            sha256 = "1891pg4x5qkh151pylvn93c4plqw6vgasa4g40jbma5xzq8pygr4";
        }
      ];
    };
  };
in
  with import <nixpkgs> {
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
