{ ... }:

{
  time.timeZone = "America/Los_Angeles";
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  services.redshift = {
    enable = true;
    brightness = {
      day = "0.8";
      night = "0.7";
    };
    temperature.day = 5700;
    temperature.night = 3500;
  };
  # Los Angeles time
  location = {
    latitude = 34.05223;
    longitude = -118.24368;
    provider = "manual";
  };
}
