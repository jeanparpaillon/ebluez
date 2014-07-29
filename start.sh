#!/bin/sh

cd $(dirname $0)

exec erl \
     -pa $PWD/ebin  \
     $PWD/deps/*/ebin \
     -lager colored true \
     -lager handlers "[{lager_console_backend, debug}]" \
     -boot start_sasl
