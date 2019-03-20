# Lunar language in docker container

build and run a container on a host
```
$ docker-compose build
$ docker-compose run lunarlang
```

compile in the container
```
# cd src
# cmake -G Ninja .
# ninja
```

remove the container on the host
```
$ docker-compose rm
```
