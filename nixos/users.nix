{ ... }:

{
  users.users.d = {
    isNormalUser = true;
    home = "/home/d";
    extraGroups = [ "wheel" "audio" "video" "optical" "storage" "disk" ];
  };
}
