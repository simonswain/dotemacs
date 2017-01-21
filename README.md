# Emacs 25

```bash
git clone git@github.com:simonswain/dotemacs.git .emacs.d
sudo apt-get install build-essential
sudo apt-get build-dep emacs24
cd /tmp
wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.gz
tar -xzvf emacs-24.5.tar.gz
cd emacs-24.5
./configure
make
sudo make install
emacs
```
