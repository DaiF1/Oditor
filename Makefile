all: run

build:
	docker build -t oditor .

run:
	docker run -a stdin -a stdout -v $(shell pwd):/app/ -it oditor
