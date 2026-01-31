build/b.o: build/a.o
build/c.o: build/a.o build/b.o
build/d.o: build/b.o
build/e.o: build/a.o build/c.o
build/d.o: build/b.o

build/d: build/d.o build/b.o build/a.o 
build/c: build/c.o build/a.o build/b.o 
build/e: build/e.o build/a.o build/c.o build/b.o 
