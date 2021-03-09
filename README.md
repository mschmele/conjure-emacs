# Emacs Config

## Installation

To install, clone this repo to `~/.emacs.d`

	cd ~
	git clone https://github.com/jaybarra/.emacs.d

Once downloaded, run Emacs. On first run it will attempt to install packages.

Some packages need additional interactive steps to configure. Specifically
any that require fonts and icons.

Run the following and restart emacs.
	
	<M-x> all-the-icons-install-fonts
	<M-x> fira-code-mode-install-fonts
    
    
### Directory Customization

You will need to modify the init.el script to point to your specific directory in the `(setq default-directory ...)` command.

## Prerequisites

There is a depedency on Fira Code Retina and Cantarrell fonts being available.
 These fonts should be installed locally before running.

* [FiraCode](https://github.com/tonsky/FiraCode)
  * [Fira Code Emacs Setup](https://github.com/jming422/fira-code-mode)
* [Cantarell](https://fonts.google.com/specimen/Cantarell)

Alternatively, update the init.el to remove those dependencies and use your own
preferred fonts.

## Font Ligatures

If you do not want font ligatures, disable or delete `fira-font-mode` in `init.el`.
