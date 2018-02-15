#!/usr/bin/env bash
DIR="app-run"
Rscript -e "RestRserve::restrserve_stop('$DIR')"
echo "cheking there are no Rserve processes:"
ps ax | grep Rserve
echo "removing app-dir ($DIR):"
rm -r $DIR
