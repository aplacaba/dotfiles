#!/bin/sh


# redis
docker run --name redis --restart always -p 6379:6379 -d redis

# postgres
docker volume create pgdata
docker run --name postgres -e POSTGRES_PASSWORD=password --restart always -d -p 5432:5432 -v pgdata:/var/lib/postgresql/data postgres:13.4

# mysql
docker volume create mysqldata
docker run --name mysql -e MYSQL_ROOT_PASSWORD=password --restart always -d -p 3307:3306 -v mysqldata:/var/lib/mysql mysql:5.7.35
