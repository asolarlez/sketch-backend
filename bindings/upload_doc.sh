#!/usr/bin/zsh
cd $(dirname $(readlink -f $0))
scp -r doc/* hz:/srv/www/htdocs/files/hscegis
