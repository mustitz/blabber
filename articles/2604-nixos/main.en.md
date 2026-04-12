# First impressions of NixOS

Looking at the aging Ubuntu 20 on my desktop, I finally came to terms with the fact that something had to change.
I'm not a big fan of upgrades, I get comfortable with what I have, but this was too much.
A good excuse to try something new though!

The question was: what exactly.
Upgrading to a new Ubuntu was out of the question.
They're already rewriting `sudo` in Rust over there. Thanks, moving on.
Gentoo? Had my fun with it, but my heart wasn't in it.
Arch? Maybe, maybe...

But... I'd been wanting to try NixOS for a long time.
The distro is very popular in the Haskell community, which is already a quality stamp.
I'd been circling around it for ages, but the nix file syntax kept putting me off.
It's essentially a separate programming language, a functional one.
That meant investing a lot of time to get up to speed.

But after reading yet another LinkedIn post about how the concepts are the hard part and the syntax is a job for LLMs, I decided the time had come!

## Introduction to NixOS

First, a few words on why NixOS specifically.
With every distro I've worked with, I kept running into the same config problem.
In Ubuntu, after years of `apt install`, `apt remove`, `autoremove`, the system gradually turns into an archaeological dig.
Somewhere at the fifth layer lies a library you installed three years ago for some project, but you're scared to remove it in case something depends on it.
Want to install a new version of a package alongside the old one? Good luck.
Want to reproduce the exact system state on another machine? Even more luck.
Snap, flatpak, docker... none of these are new ideas, just crutches for the same underlying problem.

I fought this in my own way: I'd create separate users `cat`, `hask` and install the relevant `opam` and `ghcup` there, along with the matching configs and vim plugins.
Inconvenient, but I figured it was an incurable evil.

Theoretically I knew NixOS solved these problems, but... there was never time or motivation.

In NixOS, the system is a function of its configuration files.
Instead of individual commands like `apt update`, `apt install`, you simply edit a configuration file that describes the system.
Then you run

```
sudo nixos-rebuild switch
```

and voila! The system is updated.

If you don't like the result, just run

```
sudo nixos-rebuild switch --rollback
```

and the system quickly reverts to the previous state.
NixOS works a bit like Docker here: all packages are stored in `/nix/store` under a hash.
You just configure the symlinks to them.

Of course, nothing stops you from keeping the configuration in a git repo, doing rebases to drop the unnecessary bits, sharing it with friends, copying it to new environments, and so on.
And the configs in NixOS aren't ordinary: it's a separate programming language, functional and lazy, which as a developer is nothing but a plus.
What's not to love?

## Installation

There are two main ways to install NixOS: boot from an ISO/flash drive, or run the installer from an existing Linux.
Since Ubuntu was working fine, of course I chose the second option.
By the way, `nix` is not only a distro but also a standalone package manager: you can install it on any Linux or even macOS.
That's exactly what we'll do, so here's the plan:
repartition the disks, mount them, install nix under Ubuntu, run the installer, reboot into NixOS.

### Partitioning the disks

This is a fairly routine operation, so to make it more interesting I decided to also try ZFS.
This filesystem has built-in compression, snapshots, and protection against silent data corruption.
Go big or go home.

Since we're on Ubuntu, we first install the ZFS utilities:

```bash
sudo apt install zfsutils-linux
```

Then we create a pool on the partition where we plan to install NixOS, mine is `/dev/nvme0n1p5`.

```bash
sudo zpool create \
   -O compression=lz4 \
   -O atime=off \
   -O xattr=sa \
   -O dnodesize=auto \
   -o ashift=9 \
   -O mountpoint=none \
   nixos \
   /dev/nvme0n1p5
```

`ashift=9` corresponds to a 512-byte sector size (2⁹ = 512).
Most modern drives use 4096-byte sectors and need `ashift=12`, but that's not the case here, my NVMe drives are genuinely 512-byte:

```
$ sudo nvme list
Node             SN                   Model                                    Namespace Usage                      Format           FW Rev
---------------- -------------------- ---------------------------------------- --------- -------------------------- ---------------- --------
/dev/nvme0n1     2L2929S5H7J1         XPG SPECTRIX S40G                        1           4,10  TB /   4,10  TB    512   B +  0 B   VB411D62
/dev/nvme1n1     50026B76837601F2     KINGSTON SA2000M81000G                   1         996,18  GB /   1,00  TB    512   B +  0 B   S5Z42105
```

Worth checking, because `ashift` can't be changed after the pool is created.

This command is the equivalent of formatting in other filesystems, analogous to `mkfs.ext4` or `mkfs.xfs`.
Except ZFS is a richer concept: a pool can span multiple physical disks and contain multiple independent filesystems with their own mount points.
That's why the command looks a bit more complex than plain formatting.

Next, we create datasets inside the pool:

```bash
sudo zfs create -o mountpoint=legacy nixos/root
sudo zfs create -o mountpoint=legacy nixos/nix
sudo zfs create -o mountpoint=legacy nixos/var
```

These are analogous to mount points in regular filesystems, except inside the ZFS pool.
`mountpoint=legacy` means ZFS won't mount them automatically; Linux will handle that via `/etc/fstab`.

And we mount them for installation:

```bash
sudo mount -t zfs nixos/root /mnt
sudo mkdir -p /mnt/{nix,var,boot/efi,home}
sudo mount -t zfs nixos/nix /mnt/nix
sudo mount -t zfs nixos/var /mnt/var
sudo mount /dev/nvme0n1p1 /mnt/boot/efi
sudo mount /dev/nvme0n1p3 /mnt/home
```

The EFI and home partitions are reused from Ubuntu, the rest is on ZFS.
This is standard practice when installing Linux, familiar to anyone who's installed Gentoo or Linux From Scratch.
`/mnt` becomes the root of the future system: doing a `chroot` there puts you inside it with the existing kernel.
Our `nixos` pool contains three datasets: `nixos/root`, `nixos/nix`, and `nixos/var`, each mounted separately.

### Installing nix on Ubuntu

The `nix` package manager can be installed on Ubuntu with a single command:

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

After this, you can already get a taste of `nix` right inside Ubuntu: install packages, run isolated environments, and so on...
Let's grab the tools needed to install NixOS:

```bash
nix-env -iA nixpkgs.nixos-install-tools
```

Yes, this looks like `apt install` and it really is an exception to the rules.
`nix-env` is the old approach, where packages are installed outside of the configuration.
But here we're just bootstrapping: installing a tool once to launch the installer, after which it won't be needed anymore.

### configuration.nix

Now our task is to create the main system configuration files.
You can write them by hand.
But most of it is grunt work: look at what's in the system and transcribe it.
There's a utility that does this for us:

```bash
sudo nixos-generate-config --root /mnt
```

After running it, we get two files in `/mnt/etc/nixos`.
The first, `hardware-configuration.nix`, auto-detects the hardware: disks, filesystems, CPU, kernel modules.
The second, `configuration.nix`, describes the system itself, and this is the main file we'll be editing.

The installation process was fairly straightforward from here.
I went through the generated config and asked the LLM what each option does and what it affects.
Once I understood, I picked what I liked.
What follows is a dry walkthrough of the [initial configuration](configuration.nix).
Yes, it's an example of how things get configured, but feel free to skim if it's boring.

#### Bootloader

```nix
boot.loader.systemd-boot.enable = true;
boot.loader.efi.canTouchEfiVariables = true;
```

`systemd-boot` is a simple EFI bootloader built into systemd.
An alternative to GRUB, lighter and with less fuss.

#### ZFS support

```nix
boot.supportedFilesystems = [ "zfs" ];
boot.zfs.forceImportRoot = false;
networking.hostId = "6148f1c1";
```

This is the magic to get ZFS loaded at boot.
`hostId` is required for ZFS, obtained with:

```bash
head -c 8 /etc/machine-id
```

#### nVidia setup

```nix
nixpkgs.config.allowUnfree = true;
# ...

# Nvidia
services.xserver.videoDrivers = [ "nvidia" ];
hardware.nvidia.modesetting.enable = true;
hardware.nvidia.open = false;
hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;
hardware.graphics.enable32Bit = true;
```

Here we choose the proprietary driver, because it gives us the most capabilities.
For that we need to allow closed-source software via `allowUnfree`, and set the `open` attribute to `false`.
`enable32Bit` is for Steam down the line.
The rest is cargo-cult I copied without diving into the details.

#### Audio setup

```nix
services.pipewire = {
  enable = true;
  pulse.enable = true;
  alsa.enable = true;
  alsa.support32Bit = true;
};
```

Pipewire instead of pulseaudio, the more modern option.
`support32Bit` again for Steam.

#### Locale setup

```nix
i18n.defaultLocale = "C.UTF-8";
i18n.extraLocaleSettings = {
  LC_TIME = "en_DK.UTF-8";
};
```

`C.UTF-8` is the minimal locale without any extras.
`en_DK` for ISO 8601 date format, i.e. 2026-04-12 instead of April 12.

#### Creating a user

```nix
users.users.nxmu = {
  isNormalUser = true;
  extraGroups = [ "wheel" "networkmanager" "audio" "video" ];
};
```

The standard set of groups for the only non-root user.

#### Choosing a desktop

```nix
services.desktopManager.plasma6.enable = true;
services.displayManager.sddm.enable = true;
```

Plasma 6 as the desktop, SDDM as the display manager.
The LLM picked these.

#### Minimal package set

```nix
environment.systemPackages = with pkgs; [
  vim
  git
  wget
  htop
  brave
];
```

A minimal set of packages to get started: vim, git, a browser.
Everything else gets added as needed via the config.

### Installing the system

Time to run the installer, which will build the system in `/mnt` according to our config.
The command is:

```bash
sudo -E env PATH=$PATH nixos-install
```

One nuance: we need to forward `PATH` so `sudo` can find `nix` in the current environment.
It will ask for the root password at the end.

That's it, just reboot, log into the console with `Ctrl+Alt+F2`, set the password for our user, and log in!

## Installing Steam

But a system without Steam is incomplete.
At first I had doubts about whether it would even run.
NixOS has a very non-standard directory structure: most Linux programs expect to find libraries in `/usr/lib`, `/lib`, and other familiar places, but in NixOS everything lives in `/nix/store` under a hash.

The LLM suggested some magic lines though:

```nix
programs.steam = {
  enable = true;
  gamescopeSession.enable = true;
};
hardware.steam-hardware.enable = true;
```

Added it to the config, `nixos-rebuild switch`, and Steam appeared and launched without any complaints on the first try.
This really is a good distro!

When booting the system, a new entry appeared in the bootloader menu.
Now we have a choice of what to boot:

```
Generation 1 NixOS Yarara 26.05pre971872.15c6719d8c60 (Linux 6.18.20), built on 2026-04-01
Generation 2 NixOS Yarara 26.05pre971119.8110df5ad7ab (Linux 6.18.20), built on 2026-04-01
```

Each config change creates a new generation; in our case the first generation is the old config without Steam, and the second is the new one with it.
You can boot into either one.

## Home Manager

But that was the global system configuration.
NixOS also supports per-user configuration through Home Manager.
The system config stays as is, and Home Manager complements it: it adds packages and configs only for a specific user.

Home Manager doesn't come by default, you need to install it separately:

```bash
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
```

`nix-channel` is the equivalent of PPA in Ubuntu: we connect a new repository and update the package list.
After this, `~/.config/home-manager/home.nix` appears, and that's what we edit.
Applied the usual way:

```bash
home-manager switch
```

I decided to split things like this: graphical apps are part of the global settings, while the command-line tools I spend most of my time with go through Home Manager.
For now it's just adding some packages:

```nix
{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  home.username = "nxmu";
  home.homeDirectory = "/home/nxmu";

  home.stateVersion = "25.11";

  home.packages = with pkgs; [
    python3
    colordiff
    claude-code-bin
  ];

  home.file = {};

  home.sessionVariables = {};

  programs.home-manager.enable = true;

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
```

The `flakes` line enables the experimental Nix feature of the same name: 100% reproducible environments via package hashes.
Not critical for me, package updates don't scare me.
But `flakes` adds useful commands like `nix search`, so why not.

## Shell

A separate topic that deserves some enthusiasm: `nix-shell`.
For each project you can describe an isolated environment in `shell.nix`.
It complements both the global and user-level configuration, letting you have your own tools, environment variables, and package versions for a specific project.
No more separate `cat` and `hask` users!

To enter the environment, just run `nix-shell` in the directory with `shell.nix`.
Nix will fetch all the dependencies and launch a new shell with the right tools.

Here's an example config for one of my projects:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  myNeovim = pkgs.neovim.override {
    withPython3 = true;
    extraPython3Packages = p: [ p.pynvim ];
  };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    coq_8_19
    myNeovim
    git
  ];

  shellHook = ''
    export NVIM_APPNAME=lf4nvim
    alias vim=nvim
  '';
}
```

Here we see another NixOS feature: packages can be configured and recompiled to your liking.
Unlike Gentoo, where everything compiles from source, NixOS downloads pre-built binaries by default.
But if you write a custom `override` like this, and Nix can't find that hash in the cache, it will compile it itself.
Here we're building neovim with Python support, which is needed for the Coqtail plugin, plus `pynvim` for that Python.

`shellHook` runs when entering the shell.
`NVIM_APPNAME=lf4nvim` is a neovim feature that allows separate configs for different environments: neovim will look for its config in `~/.config/lf4nvim/` instead of the default `~/.config/nvim/`.
Plugins will also be looked up in `~/.local/share/lf4nvim/`.
So in those directories we can configure plugins and settings specifically for this shell.
The plugin can be installed with:

```bash
mkdir -p ~/.local/share/lf4nvim/site/pack/vendor/start/
git clone https://github.com/whonore/Coqtail ~/.local/share/lf4nvim/site/pack/vendor/start/coqtail
```

And a bit of tweaking in `~/.config/lf4nvim/init.lua`:

```lua
vim.cmd("source ~/.vimrc")

vim.opt.mouse = ""

vim.keymap.set('n', '<F9>', ':CoqStart<CR>', { silent = true })
vim.keymap.set('n', '<F4>', ':CoqToLine<CR>', { silent = true })
vim.keymap.set('n', '<Space>', ':CoqNext<CR>', { silent = true })
vim.keymap.set('n', '<BS>', ':CoqUndo<CR>', { silent = true })
```

As a bonus, this `shell.nix` can also be used on macOS.
Set up the `lf4nvim` profile for neovim, install `nix`, run `nix-shell` and... voila, it all works on macOS!

## Summary

NixOS grew on me, and I hope it will be my next production desktop OS.
Compared to Ubuntu, there's a feeling of a clean slate.
The system boots fast, there are few services running, and dmesg isn't cluttered with dozens of things you never asked for.
The syntax turned out to be easier than I expected, probably my Haskell background helped.
LLMs are great for figuring out the configuration, it would've been harder without one.
There's a risk with software that has no packages: due to the non-standard directory structure, installing something manually can be trickier than on Ubuntu.
But I haven't run into that yet.
And the community has packaged enough, over 100,000 packages, which is more than even Arch.
Overall, `nix-shell` is a great alternative to Docker for environment isolation: it doesn't mess up your global configuration, and it's quick and easy to modify.
