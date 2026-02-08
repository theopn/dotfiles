{ lib, pkgs, ... }:
  let username = "theopn";
  in {
    xdg.stateHome = "/home/${username}/.local/share";
    home = {
      inherit username;
      homeDirectory = "/home/" + username;

      # packages = with pkgs; [
      # ];

      file = {
        ".config/lf".source = ./lf/.config/lf;
      };

      stateVersion = "25.11";
    };

    programs.home-manager.enable = true;

    programs.bat.enable = true;

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
