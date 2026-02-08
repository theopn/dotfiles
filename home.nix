{ lib, pkgs, ... }:
  let username = "theopn";
  in {
    xdg.stateHome = "/home/${username}/.local/share";
    nixpkgs.config.allowUnfree = true;
    home = {
      inherit username;
      homeDirectory = "/home/" + username;

      packages = with pkgs; [
        # CLI Tools
        bat
        chafa
        exiftool
        fastfetch
        ffmpeg
        figlet
        git-filter-repo
        keychain
        hugo
        imagemagick
        #tailscale
        tmux
        tree
        wget

        # Vim
        fd
        fzf
        lua
        neovim
        neovide
        ripgrep
        vim

        # Tex
        texlive.combined.scheme-full
        poppler
        zathura
        zathuraPkgs.zathura_pdf_poppler

        # PL
        nodejs_24
        R
        cargo
        rustc

        # GUI apps
        zotero

        # Propritery apps
        chromium
        discord
        #kicad
        slack
        spotify
        #zoom-us
      ];

      file = {
        ".config/lf".source = ./lf/.config/lf;
        ".config/fastfetch".source = ./fastfetch/.config/fastfetch;
        ".tmux.conf".source = ./tmux/.tmux.conf;
      };

      stateVersion = "25.11";
    };  # home

    programs.home-manager.enable = true;

    programs.btop = {
      enable = true;
      settings = {
        color_theme = "TTY";
      };
    };

    programs.lf = {
      enable = true;
    };

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        ignoreUserConfig = true;
        addons = with pkgs; [ fcitx5-hangul fcitx5-mozc ];
        settings = {
          inputMethod = {
            GroupOrder."0" = "Default";
            "Groups/0" = {
              Name = "Default";
              "Default Layout" = "us";
              DefaultIM = "hangul";
            };
            "Groups/0/Items/0".Name = "keyboard-us";
            #"Groups0/Items/1".Name = "hangul";
          };
        };
      };
    };   # i118n

  }
