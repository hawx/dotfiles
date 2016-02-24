# Dotfiles

### emacs.d

Started with [overtone/live-coding-emacs][liv] then threw in a bit of
[defunkt/emacs][def] and bits from the emacswiki. There may be a bit of original
.el, in there but not much.

### git

A patchwork from various others I've seen, with some aliases of mine, which are
probably not original (regardless, `git cb new-branch` speeds up _everything_).

### zsh

This started of as [spicycode/ze-best-zsh-config][zsh] over time I have modified
it.


## Install

    stow -t ~ emacs
    stow -t ~ git
    stow -t ~ ruby
    stow -t ~ upstart
    stow -t ~ zsh

[zsh]: https://github.com/spicycode/ze-best-zsh-config
[liv]: https://github.com/overtone/live-coding-emacs
[def]: https://github.com/defunkt/emacs
