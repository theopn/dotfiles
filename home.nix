{ lib, pkgs, ... }:
  let username = "theopn";
  in {
    xdg.stateHome = "/home/${username}/.local/share";
    home = {
      inherit username;
      homeDirectory = "/home/" + username;

      packages = with pkgs; [
        bat
        chafa
        exiftool
        fastfetch
        fd
        ffmpeg
        figlet
        fzf
        git-filter-repo
        hugo
        imagemagick
        lua
        neovim
        nodejs_24
        R
        ripgrep
        cargo
        rustc
        tree
        tmux
        vim
        wget

        neovide


        texlive.combined.scheme-full
      ];

      file = {
        ".config/lf".source = ./lf/.config/lf;
        ".config/fastfetch".source = ./fastfetch/.config/fastfetch;
        ".tmux.conf".source = ./tmux/.tmux.conf;
      };

      stateVersion = "25.11";
    };

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

  }
