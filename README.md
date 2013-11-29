# Gary Fredericks' Dotfiles

## Bootstrapping

Add to .bashrc:

```
# List of different dotfiles dirs, separated by colons
PROFILES=~/dotfiles
# Run the bootstrap to create the symlinks (looks for
# files/dirs that end in .symlink)
source ~/dotfiles/bootstrap
# Run all other bashrc files.
source ~/.bashrc.*
```