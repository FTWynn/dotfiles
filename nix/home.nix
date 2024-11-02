{ config, pkgs, pkgs-unstable, ... }:

let
  #dots = "/home/david/dotfiles";
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "david";
  home.homeDirectory = "/home/david";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = (with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    thunderbird
    #gnome-tweaks
    obsidian
    alacritty
    kitty
    ptyxis
    fava
    atuin
    lazygit
    lazydocker
    spotify
    # Project Bluefin Suggestions
    fd
    fzf
    ripgrep
    tealdeer
    ugrep
    yq
    zoxide
    fastfetch
    #qsyncthingtray
    syncthingtray
    k9s
    flameshot
    quarto
  ])
  ++
  (with pkgs-unstable; [
    #zed-editor
    #vscode
    _1password-gui
    gh
  ]);

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # FTWynn: Testing this out as stow alternative
    ".config/starship.toml".source = ../starship/starship.toml;
    ".bashrc".source = ../bash/.bashrc;
    ".config/fish/config.fish".source = ../fish/config.fish;
    ".config/kitty/kitty.conf".source = ../kitty/kitty.conf;
    ".emacs.d" = {
      source = ../.emacs.d;
      recursive = true;
    };

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/david/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  # FTWynn : It is unclear to me why this is sometimes required beyond having the packages just plain installed
  # programs.vscode = {
  #   enable = true;
  # };


  programs.git = {
    enable = true;
    userName = "David Wynn";
    userEmail = "Remixer96@gmail.com";
  };

  programs.starship.enable = true;
  programs.direnv.enable = true;
  programs.eza.enable = true;

  services.syncthing = {
    enable = true;
    tray.enable = true;
    tray.command = "syncthingtray";
  };

  services.redshift = {
    enable = true;
    provider = "manual";
    # Atlanta Home
    longitude = -84.4226425;
    latitude = 33.8369766;
    temperature.night = 1500;
  };
}
