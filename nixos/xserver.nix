{ pkgs, ... }:

{
  services.xserver = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
	i3status
      ];
    };
    windowManager.i3.package = pkgs.i3-gaps;
  };

  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e,caps:escape";
}
