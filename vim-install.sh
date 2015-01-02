mkdir -p ~/.vim/bundle
git clone https://github.com/Shougo/neobundle.vim.git ~/.vim/bundle/neobundle.vim

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE:-$0}")"; pwd)"
pushd $script_dir
ln -s .vimrc ~/
ln -s .gvimrc ~/
popd 
