#!/bin/sh

openssl genrsa -out "$1-priv.pem" 1024 ; 
openssl rsa -in "$1-priv.pem" -pubout -out "$1-pub.pem"