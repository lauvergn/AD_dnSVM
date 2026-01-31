# `mk-fdeps`

A tool that extracts dependencies between Fortran program units across multiple source files and generates corresponding Make recipes. Only free format is supported for now. Preprocessing and submodules are supported but not yet widely tested, please report any bugs. 

Sample output:
```
$ mk-fdeps test/basic/**/*.f90 --include-targets
build/basic/b.o: build/basic/a.o
build/basic/c.o: build/basic/a.o build/basic/b.o
build/basic/d.o: build/basic/b.o
build/basic/e.o: build/basic/a.o build/basic/c.o
build/basic/subdir/d.o: build/basic/b.o

build/basic/c: build/basic/c.o build/basic/a.o build/basic/b.o
build/basic/d: build/basic/d.o build/basic/b.o
build/basic/e: build/basic/e.o build/basic/a.o build/basic/c.o
```

One may redirect the output to a file:
```
$ mk-fdeps test/basic/**/*.f90 --include-targets --output deps.mk
```
so they can be included in a Makefile:
```Makefile
FC=gfortran
FC_FLAGS=-O0 -g -fbounds-check -Jbuild

build:
        mkdir -p $@

build/%.o: %.f90 | build
        $(FC) $(FC_FLAGS) -c $^ -o $@

build/%: | build
        $(FC) $(FC_FLAGS) $^ -o $@

# import generated recipes
include ./deps.mk 
```

## Usage 

```
$ mk-fdeps --help
usage: mk-fdeps [source / option] ...
 Options:
       --with-prefix  Add prefix to the file stem: parent/[prefix]stem.ext
                      type: string
       --with-suffix  Add suffix to the file stem: parent/stem[suffix].ext
                      type: string
       --with-parent  Add parent to the whole path: [new-parent]parent/stem.ext
                      type: string, default: build/
          --with-ext  Replace the file extension: parent/stem.[ext]
                      type: string, default: .o
     --strip-parents  Remove the first 'n' parents of the original path
                      type: integer, default: 1
   --include-targets  Generate rules for files with programs in it
                      type: bool, default: absent
            --output  Output location
                      type: string
        --preprocess  Run preprocessing beforehand
                      type: bool, default: absent
  --preprocessor-cmd  Preprocessor command to use
                      type: string, default: 'gfortran -cpp -E'
```

## Installation

```
make all
PREFIX=~/.local/bin make install
```
