all:: build

build: hello copy

hello:
	cabal build

copy:
	[ -d bin ] || mkdir bin
	cp dist/build/hello-world/hello-world bin/hello-world
