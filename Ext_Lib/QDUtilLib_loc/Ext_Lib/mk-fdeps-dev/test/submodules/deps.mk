build/main.o: build/module.o build/submodule.o
build/submodule.o: build/module.o

build/main: build/main.o build/module.o build/submodule.o
