<p align="center">
<img src="/home/.config/idempotent.png" width="30%" />
</p>
<p align="center">
    <b> λ Idempotent Dotfiles</b>
</p>

****

# SETTING UP NIXOS , MY WAY
[![NixOS Unstable](https://img.shields.io/badge/NixOS-unstable-blue.svg?style=for-the-badge&logo=NixOS&logoColor=white)](https://nixos.org)
[![Idempotent Dots](https://img.shields.io/github/last-commit/sam1431/IdempotentDots?style=for-the-badge)](https://github.com/Sam1431/IdempotentDots)
[![Repo](https://img.shields.io/github/repo-size/sam1431/IdempotentDots?label=repo&style=for-the-badge)](https://github.com/Sam1431/IdempotentDots)
[![License](https://img.shields.io/github/license/sam1431/Idempotentdots?style=for-the-badge)](https://github.com/Sam1431/IdempotentDots)

NixOS with tmpfs (Sway WM)

> **Disclaimer:** _This is not a community framework or distribution._ It's a
> private configuration and an ongoing experiment while I feel out NixOS. I make
> no guarantees that it will work out of the box for anyone but myself. It may
> also change drastically and without warning. 

<img src="/home/.config/win.png" width="100%" />

<p align="center">
<span><img src="/home/.config/ide.png" height="178" /></span>
<span><img src="/home/.config/menu.png" height="178" /></span>
<span><img src="/home/.config/eww.png" height="178" /></span>
</p>

**[NVIM AND EWW CONFIG](https://github.com/Sam1431/IdempotentDots/tree/main/home/.config)**

------

| | |
|-|-|
| **Shell:** | Zsh + Starship |
| **DM:** | GreetD (tuigreet) |
| **WM:** | Sway + Waybar + Eww |
| **Editor:** | Neovim|
| **Terminal:** | Foot (client-daemon mode) |
| **Launcher:** | Wofi |
| **Browser:** | Chromium |
| **GTK Theme:** | Nordic |

-----

# Lets Begin
## Flashing the Iso

1. - Acquire NixOS 21.11 or newer [here](https://nixos.org/download.html)
    -  Write it to a flash drive `dd if=<ur-iso>.iso of=/dev/sdXXX bs=4k status=progress `

2. - Boot into the installer.
    - ## Setting up tmpfs
 - for legacy 
 ```
# Defining a helper variable to make the following commands shorter.
DISK=/dev/disk/by-id/ata-VENDOR-ID-OF-THE-DRIVE

# Create partition table
parted $DISK -- mklabel msdos

# Create a /boot as $DISK-part1
parted $DISK -- mkpart primary ext4 1M 512M
parted $DISK -- set 1 boot on

# Create a /nix as $DISK-part2
parted $DISK -- mkpart primary ext4 512MiB 100%
```
- for efi
```
# Defining a helper variable to make the following commands shorter.
DISK=/dev/disk/by-id/ata-VENDOR-ID-OF-THE-DRIVE

# Create partition table
parted $DISK -- mklabel gpt

# Create a /boot as $DISK-part1
parted $DISK -- mkpart ESP fat32 1MiB 512MiB
parted $DISK -- set 1 boot on

# Create a /nix as $DISK-part2
parted $DISK -- mkpart Nix 512MiB 100%
```

### Step 2 - Creating the file systems
```
# /boot partition for legacy boot
mkfs.ext4 $DISK-part1

# /boot partition for EFI
mkfs.vfat $DISK-part1

# /nix partition
mkfs.ext4 $DISK-part2
```
### Step 3 - Mounting the file systems

```
# Mount your root file system
mount -t tmpfs none /mnt

# Create directories
mkdir -p /mnt/{boot,nix,etc/nixos,var/log}

# Mount /boot and /nix
mount $DISK-part1 /mnt/boot
mount $DISK-part2 /mnt/nix

# Create a directory for persistent directories
mkdir -p /mnt/nix/persist/{etc/nixos,var/log}

# Bind mount the persistent configuration / logs
mount -o bind /mnt/nix/persist/etc/nixos /mnt/etc/nixos
mount -o bind /mnt/nix/persist/var/log /mnt/var/log

```
4. Now go ahead and do a  `nixos-generate-config --root /mnt`  to get a basic configuration for your system.

5. `# git clone https://github.com/hlissner/dotfiles /mnt/etc/nixos/repo`
    ` # mv /mnt/etc/nixos/repo/nixos/* /mnt/etc/nixos/`

6. cofigure your host under /mnt/etc/nixos/hosts with config you obtained from **step 4** ( for a tmpfs layout check [this](https://github.com/Sam1431/IdempotentDots/blob/main/etc/nixos/modules/system/hardware.nix) out ). Also require the host file from [config.nix](https://github.com/Sam1431/IdempotentDots/blob/main/etc/nixos/config.nix)

8. make flakes available to nixos - ` nix-shell -p git nixFlakes ranger neovim`

**NOTE**

- the machine won't remember your password after reboot if you use 'passwd'
- rather 
``` config 
     users.mutableUsers = false;
     # $ nix-shell --run 'mkpasswd -m SHA-512 -s' -p mkpasswd
     users.users.root.initialHashedPassword = "the has you got from the above command";
```

   #### Install nixOS
    `# nixos-install --no-root-passwd --flake /mnt/etc/nixos`

also shown [here](https://github.com/Sam1431/IdempotentDots/blob/main/etc/nixos/users/sam.nix)

9. Then reboot

## Management

`/bin/znx`, Inspired by hlissner's /bin/hey ( but written in bash, way more messy and less functionality )

```
Usage: znx [flags]

   # SYSTEM MANAGEMENT
   rebase / switch - rebuild NixOS
   upgrade - upgrade NixOS
   conf-edit / conf-ed - list all System Config

   # FLAKE MANAGEMENT
   flake-edit / flk-ed - edit Flake config    
   flake-lock / flk-lc - recreate system flake lock [ not available ]
   flake-update / flk-up - update root flake

   # MANUAL GARBAGE COLLECTIONS
   garbage-collect / gc - clean nix store home
   garbage-collect -d / gcd - clean nix store root
   
```

+ **Why tmpfs?**
 because I have something similar to a [OCD for perfection](https://www.treatmyocd.com/blog/just-right-perfectionism-ocd) and I want my system to be as clean as possible
  
 I use tmpfs for /home as mentioned [here](https://elis.nu/blog/2020/06/nixos-tmpfs-as-home/) 
by creating /home/sam under /nix/persist and giving the user read-write permission to that directory

+ [nixos tmpfs /home](https://elis.nu/blog/2020/06/nixos-tmpfs-as-home/)
+ [nixos tmpfs as root](https://elis.nu/blog/2020/05/nixos-tmpfs-as-root/)
+ [nixos](https://releases.nixos.org/?prefix=nixos/unstable/)
+ [agenix](https://github.com/ryantm/agenix)
+ [hlissner dotfiles](https://github.com/hlissner/dotfiles)
+ [home-manager](https://github.com/nix-community/home-manager)
+ [flakes](https://nixos.wiki/wiki/Flakes)
+ [eww config modified from](https://github.com/johnsci911/eww.widgets)


**PS** neovim config doesn't work properly but will be fixed soon

-----

# NOW TO THE DESKTOP PART

- [~/.swayrc](https://github.com/Sam1431/IdempotentDots/blob/main/home/.swayrc) is the autostart script
- it is also used the waybar theme from [here](https://github.com/Sam1431/IdempotentDots/tree/main/etc/nixos/home/modes)
- ~/.config/nvim , ~/.config/eww ~/.swayrc ~/any-dir-to-store-ur-files-and-wall need to persistent for config to work
- sway theme is declared at [/etc/nixos/config/themes/generic.nix](https://github.com/Sam1431/IdempotentDots/blob/main/etc/nixos/config/themes/generic.nix)
- the main sway config is located at [/etc/nixos/config/sway/sway.nix](https://github.com/Sam1431/IdempotentDots/blob/main/etc/nixos/config/sway/sway.nix)
