all: run

build:
	docker build -t oditor .

run:
	docker run -a stdin -a stdout -v $(pwd):/app/ -it oditor
