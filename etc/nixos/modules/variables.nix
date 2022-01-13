{
 services = import ./services/services.nix;
 security = import ./security/security.nix;
 blacklist = import ./security/blacklist.nix;
 packages = import ./packages/packages.nix;
 audio = import ./audio/audio.nix;
 bluetooth = import ./bluetooth/bluetooth.nix;
 desktop.sway = import ./desktop/sway.nix;
 desktop.exwm = import ./desktop/exwm.nix;
 display.gdm = import ./desktop/gdm.nix;
 hardware = import ./system/hardware.nix;
 network = import ./system/network.nix;
}
