with import <nixpkgs> { };
[
  (hiPrio clang)
  (hiPrio gcc_multi)
  bat
  binutils-unwrapped
  cacert # required on MacOS (otherwise NIX_SSL_CERT_FILE will be unset)
  cloc
  curl
  diff-so-fancy
  diffutils # built-in `diff` on MacOS is archaic
  docker
  docker-compose
  fzf
  git
  git-absorb
  git-lfs
  github-cli
  gnugrep
  gnumake
  grep
  htop
  hub
  jq
  lnav
  mons
  moreutils # provides `vipe`, `vidir` etc.
  ncdu
  neovim
  nix
  nixfmt
  ranger
  ripgrep
  rsync
  rustup
  stack
  stow
  tectonic
  tmux
  unclutter
  wget
  yarn
  zsh
] ++ (if system != "aarch64-darwin" then [
  bubblewrap # used by `opam`
  strace
  polybar
] else
  [ fswatch ])
