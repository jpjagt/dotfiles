DOTFILES_DIRECTORY=$(cd `dirname $0` && pwd)

ln -s $DOTFILES_DIRECTORY/.emacs.d ~/.emacs.d
ln -s $DOTFILES_DIRECTORY/zshrc ~/.zshrc

# global gitignore
ln -s $DOTFILES_DIRECTORY/.gitignore_global ~/.gitignore_global
git config --global core.excludesfile ~/.gitignore_global
