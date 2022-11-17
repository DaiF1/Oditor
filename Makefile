DIR = 
UID =
GID =

ifeq ($(OS),Windows_NT)
	DIR = ${CURDIR}
	UID = 0
	GID = 0
else
	DIR = $(shell pwd)
	UID = $(shell id -u)
	GID = $(shell id -g)
endif

all: run

build:
	docker build -t oditor \
		--build-arg UID=${UID} \
		--build-arg GID=${GID} .

run:
	docker run -a stdin -a stdout -v ${DIR}:/app/ \
		-it oditor

clean:
	docker container rm $(shell docker ps -qa)
