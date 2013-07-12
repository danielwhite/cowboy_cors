#!/bin/sh
erl -pa ebin ../ebin ../deps/*/ebin -s example \
    -eval "io:format(\"Point your browser at http://localhost:8080/\")."
