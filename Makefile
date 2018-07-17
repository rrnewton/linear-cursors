

build:
	docker build -t mylinear .
	stack build

run: build
	stack exec -- linear-cursors.exe
