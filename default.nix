/*
 * Note: You need to add one or more non-core dependencies to your
 * cabal file in order for nix-shell to work (reflex-dom, text, etc)
 */
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  /*
   * Uses jsaddle by default, which provides faster reloading
   * than compiling to GHCJS. Comment this line out if you want
   * to build to GHCJS.
   */
  useWarp = true;

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
