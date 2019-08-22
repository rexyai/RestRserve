#!/usr/bin/env bash

SSL_CERT_DIR="$PWD/cert/"
mkdir -p $SSL_CERT_DIR
openssl req \
  -newkey rsa:2048 \
  -x509 \
  -nodes \
  -keyout $SSL_CERT_DIR/server.key \
  -new \
  -out $SSL_CERT_DIR/server.cert \
  -subj /CN=localhost \
  -sha256 \
  -days 3650
