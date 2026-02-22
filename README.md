# Dotfiles

Configuration files for various programs. Managed by GNU [stow](https://www.gnu.org/software/stow/).

This repository currently contains configuration for the following programs:

| Program | Description |
| --- | --- |
| `aerospace` | Tiling window manager for macOS. The most i3-like I can find. |
| `aliases` | Shell aliases, shared across environments. |
| `claude` | Configuration for [Claude Code](https://docs.anthropic.com/en/docs/claude-code). |
| `ghostty` | Currently my preferred terminal. |
| `git` | Aliases and custom command configuration. |
| `karabiner` | Keyboard remapping via [Karabiner-Elements](https://karabiner-elements.pqrs.org/). |
| `scripts` | A bunch of utility scripts: git helpers, tmux integration, screenshot tools, etc. |
| `tmux` | Terminal multiplexer + copy to clipboard via keyboard. |
| `vim` | Neovim configuration. Spacemacs didn't stick. |

## Installation

```sh
make install    # symlink all packages into ~
make uninstall  # reverse the symlinks
make lint       # shellcheck all shell files
```
