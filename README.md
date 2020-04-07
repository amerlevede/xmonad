This folder uses stack to configure and build xmonad.
It contains several Haskell projects:
 - xmonad and xmonad-contrib, copied from git, only modified to avoid building the default executable.
 - xmonad-my, a personal library for xmonad functionality.
 - config-[computer], the project that builds the required executable(s).

Each computer should build a different config project suitable for that system.


Sync instructions
-----------------
To use this setup for xmonad, sync the Seafile library to any folder (not necessarily ~/.xmonad).
The seafile-ignore.txt file avoids syncing compilation files and binaries.

Configure instructions
-----------------
This library is intended to sync xmonad code, but not machine-local settings and compilations.
Each computer has its own config-* folder, with a different project.

The xmonad.hs file (and perhaps other executables) provides code to build the window manager. Since all the working code is imported from the xmonad, xmonad-contrib and xmonad-my libraries, it is essentially a configuration file.
The stack.yaml file tells stack where to look for shared xmonad code.
The config.cabal file tells stack which executables to compile (should at least include xmonad.hs), and which libraries they depend on.

The config-* folder may include auxilliary files or additional Haskell modules/executables.

Build instructions
-----------------
Compile using:
 > cd $THISFOLDER/config-$THISCOMPUTER
 > stack install
to install the xmonad executable to ~/.local/bin/xmonad.
or
 > cd $THISFOLDER/config-$THISCOMPUTER
 > stack build
to let stack manage the executable in the .stack-work subdirectory of the project.

Both methods will automatically compile the xmonad, xmonad-contrib and xmonad-my local libraries, before installing the executable.
The xmonad libraries depend on some (non-Haskell) X libraries, which need to be installed in the machine.

In Ubuntu:
> sudo apt install libx11-dev libxrandr-dev pkg-config libxft-dev
In OpenSUSE:
> sudo zypper install libXrandr
(Lists of dependencies are likely incomplete; update when necessary)

Taffybar also requires:
In Ubuntu:
> sudo apt install libasound2-dev libxml2-dev libcairo2-dev libpango1.0-dev libgtk2.0-dev libglib2.0-dev libgtk-3-dev

Depending on the configuration, other external packages may be required.

Use instructions
------------------
Run using:
 > xmonad
assuming the local xmonad config was compiled using stack install and ~/.local/bin is on the $PATH.
Otherwise, run:
 > cd $THISFOLDER/config-$THISCOMPUTER
 > stack exec -- xmonad
assuming the local xmonad config was compiled using either stack install or stack build.

If another window manager is already running, add --replace to the command.

xmonad is best added as an xsession option in the login manager.
To do this, add an xsession .desktop file in /usr/share/xsessions.
