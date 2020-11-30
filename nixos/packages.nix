{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wget
    neovim
    xorg.xdpyinfo
    firefox
    git
    pa_applet
    lxappearance
    redshift
    sbcl
  ];

  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      fira-code
      fira-code-symbols
    ];
    fontconfig = {
      penultimate.enable = false;
      antialias = true;
      hinting.autohint = false;
    };
  };

  # TODO(DarinM223): Add this when there is enough disk space.
  # users.users.d.packages = with pkgs;
  #   let
  #     extensions = (with pkgs.vscode-extensions; [
  #         ms-python.python
  #       ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
  #       # {
  #       #   name = "remote-ssh-edit";
  #       #   publisher = "ms-vscode-remote";
  #       #   version = "0.47.2";
  #       #   sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
  #       # }
  #     ];
  #     vscodium-with-extensions = pkgs.vscode-with-extensions.override {
  #       vscode = pkgs.vscodium;
  #       vscodeExtensions = extensions;
  #     };
  #   in
  #     [
  #       vscodium-with-extensions
  #     ];

  users.users.d.packages = with pkgs; [
    vscodium
  ];
}
