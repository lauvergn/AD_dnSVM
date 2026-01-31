PREFIX ?= /usr/local/bin

FC=gfortran
FC_FLAGS=-O0 -g -fbounds-check -Jbuild

build:
	mkdir -p $@

build/%.o: %.f90 | build
	$(FC) $(FC_FLAGS) -c $< -o $@

build/%: | build
	$(FC) $(FC_FLAGS) $^ -o $@

include ./deps.mk
