

all:
	docker build -t mylinear .
	stack build
