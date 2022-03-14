# Emacs Config

## Installation

To install, clone this repo to `~/.emacs.d`

	cd ~
	git clone https://github.com/jaybarra/.emacs.d

Once downloaded, run Emacs. On first run it will attempt to install packages.

Some packages need additional interactive steps to configure. Specifically
any that require fonts and icons.

Run the following and restart emacs.
	
    <M-x> install-package use-package
    <M-x> install-pakcage paradox
    <M-x> all-the-icons-install-fonts
    
    
### Directory Customization

You will need to modify the init.el script to point to your specific directory in the `(setq default-directory ...)` command.


### Prerequisites

* [MesloLGS NF Fonts](https://github.com/romkatv/powerlevel10k/blob/master/font.md)
