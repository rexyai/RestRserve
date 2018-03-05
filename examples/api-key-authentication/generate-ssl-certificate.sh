#!/usr/bin/env bash

SSL_CERT_DIR="run/cert/"
mkdir -p $SSL_CERT_DIR
openssl req  -nodes -new -x509  -keyout $SSL_CERT_DIR/server.key -out $SSL_CERT_DIR/server.cert

# alternatively can use something like this:

#openssl genrsa -out $CERTDIR/server.key 2048
#openssl req -new -key $CERTDIR/server.key -out $CERTDIR/server.ca
#openssl x509 -req -days 365 -in $CERTDIR/server.ca -signkey $CERTDIR/server.key -out $CERTDIR/server.cert

chmod +x "1-deploy.R" "2-start.R" "3-test.R" "4-stop.R"
