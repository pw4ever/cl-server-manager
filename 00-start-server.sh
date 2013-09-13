#!/bin/sh

NAME=cl-server-manager
sbcl --noinform --lose-on-corruption --end-runtime-options --eval "(ql:quickload :${NAME})" --eval "(in-package :${NAME})" --eval "(launch-system-with-defaults t :console)" --end-toplevel-options $*
