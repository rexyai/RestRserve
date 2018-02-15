#!/usr/bin/env bash
DIR="app-run"
Rscript -e 'source("1-train.R")' $DIR
Rscript -e 'source("2-serve.R")' $DIR

REQ=$DIR"/request.json"
echo "trying curl request file $REQ"
echo "-----------------------------------------------"

RESULT=`curl -H "Content-Type: application/json" --data @$REQ  http://localhost:8001/predict`

echo "-----------------------------------------------"
echo "result = $RESULT"
