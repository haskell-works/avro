let 
  vscode-overlay = self: super: {
    vscode-with-extensions = super.vscode-with-extensions.override {
      vscodeExtensions = with super.vscode-extensions; [
        bbenoist.Nix
      ] ++ super.vscode-utils.extensionsFromVscodeMarketplace [
        {
            name = "language-haskell";
            publisher = "justusadam";
            version = "2.7.0";
            sha256 = "1z6nxbg1a0yvbdicib3kxl04hrxwxi3p1hmc0qfahqkf6xwcmlc5";
        }
        {
            name = "vscode-hie-server";
            publisher = "alanz";
            version = "0.0.34";
            sha256 = "0cipm36l3219r1yhk4j7l02mc2c0chfnv7wl44n1h0966jp1sda3";
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

      vivaldi
      vivaldi-widevine
      vivaldi-ffmpeg-codecs
      
    ];

    shellHook = ''
      PATH=~/.cabal/bin:$PATH
      LD_LIBRARY_PATH=${pkgs.zlib}/lib/:$LD_LIBRARY_PATH
    '';
  }
