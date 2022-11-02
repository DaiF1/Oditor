all: run

build:
	docker build -t oditor \
		--build-arg UID=$(shell id -u) \
		--build-arg GID=$(shell id -g) .

run:
	docker run -a stdin -a stdout -v $(shell pwd):/app/ \
		-it oditor
