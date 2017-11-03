#!/bin/sh

docker run --name suavemusicstore_db -e POSTGRES_PASSWORD=mysecretpassword -d -p 5432:5432 theimowski/suavemusicstore_db:0.1
