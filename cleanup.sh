#!/bin/bash
# MDS - Script
#
# Limpeza do ambiente.
#
# autor: Marcelo dos Santos
#
read -p "Deseja remover o diretório 'elpa' (s/n)? "
if [ "$REPLY" == "s" ]; then
    rm -rv ~/.emacs.d/elpa
fi

read -p "Deseja remover o diretório '.cache' (s/n)? "
if [ "$REPLY" == "s" ]; then
    rm -rv ~/.emacs.d/.cache
fi
