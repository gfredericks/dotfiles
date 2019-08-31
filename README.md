# Gary Fredericks' Dotfiles

## Bootstrapping

Add to `.bashrc`:

```
DOTFILES_DIR=~/dotfiles
# Args are directories to look for dotfiles in (in particular looks
# for files/dirs that end in .symlink). Add extra args here to
# incorporate other dotfiles directories.
$DOTFILES_DIR/bootstrap $DOTFILES_DIR/base
# Run all other bashrc files.
for bashrc in $(ls ~/.bashrc.* | sort); do
  source $bashrc
done
```