# -*- mode: ruby; -*-

tap "homebrew/bundle"
tap "homebrew/cask"
tap "homebrew/core"
tap "candid82/brew" # joker
tap "homebrew/cask-versions" # jdk8
tap "borkdude/brew" # clj-kondo

cask "emacs"
cask "wireshark"
cask "aerial"
cask "keybase"
cask "intel-power-gadget" # For CPU frequency in iStat Menus
cask "mattermost"

mas "Things", id: 904280696
mas "Bear", id: 1091189122

## Services
cask "homebrew/cask-versions/adoptopenjdk8" # Needed for ES...
brew 'elasticsearch', restart_service: :changed
brew "postgresql@9.6", restart_service: :changed, link: true
brew 'redis', restart_service: :changed
brew 'riemann', restart_service: :changed

## Dev tools
brew "aspell"
brew "bash"
brew "bash-completion@2"
brew "bat"
brew "cmake"
brew "borkdude/brew/clj-kondo"
brew "clojure"
brew "clojure-lsp"
brew "curl"
brew "diff-so-fancy"
brew "fd"
brew "fzf"
brew "git"
brew "htop"
brew "hub"
brew "joker"
brew "jq"
brew "leiningen"
# For the system Java wrappers to find this JDK, symlink it with
#  sudo ln -sfn /usr/local/opt/openjdk@11/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-11.jdk
brew "openjdk@11"
brew "pass"
brew "ripgrep"
brew "shellcheck"
brew "terminal-notifier"
brew "the_silver_searcher"
brew "tmux"
brew "tree"
brew "vim"
brew "watch"

## Other
brew "rustup"
brew "zola" # static site generator
brew "restic" # backups!
