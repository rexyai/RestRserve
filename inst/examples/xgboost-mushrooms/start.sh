#!/usr/bin/env bash
DIR="run"
REQ=$DIR"/request.json"

Rscript train.R $DIR

echo "-----------------------------------------------"
echo "Starting serice. Try to query with:"
echo "curl -H \"Content-Type: application/json\" --data @$REQ  http://localhost:8001/predict"
echo "-----------------------------------------------"

Rscript app.R $DIR
