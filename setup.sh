# global gitignore
rm ~/.gitignore_global
ln -s ~/code/jpjagt/dotfiles/.gitignore_global ~/.gitignore_global
git config --global core.excludesfile ~/.gitignore_global
