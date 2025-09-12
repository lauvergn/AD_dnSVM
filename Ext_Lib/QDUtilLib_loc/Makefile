#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                gfortran (version: 9.0 linux and osx)
 FC = gfortran
#
# Optimize? Empty: default Optimization; 1: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
## force the default integer (without kind) during the compillation.
## default 4: , INT=8 (for kind=8)
INT = 4
## change the real kind
## default real64: , possibilities, real32, real64, real128
RKIND = real64
# For some compilers (like lfortran), real128 (quadruple precision) is not implemented
# WITHRK16 = 1 (0) compilation with (without) real128
WITHRK16 = 
#=================================================================================
ifeq ($(FC),)
  FFC      := gfortran
else
  FFC      := $(FC)
endif
ifeq ($(OPT),)
  OOPT      := 1
else
  OOPT      := $(OPT)
endif
ifneq ($(OOPT),$(filter $(OOPT),0 1))
  $(info *********** OPT (optimisation):        $(OOPT))
  $(info Possible values: 0, 1)
  $(error ERROR: Incompatible options)
endif
ifeq ($(OMP),)
  OOMP      := 1
else
  OOMP      := $(OMP)
endif
ifneq ($(OOMP),$(filter $(OOMP),0 1))
  $(info *********** OMP (openmp):        $(OOMP))
  $(info Possible values: 0, 1)
  $(error ERROR: Incompatible options)
endif
ifeq ($(LAPACK),)
  LLAPACK      := 1
else
  LLAPACK      := $(LAPACK)
endif
ifneq ($(LLAPACK),$(filter $(LLAPACK),0 1))
  $(info *********** LAPACK:        $(LLAPACK))
  $(info Possible values: 0, 1)
  $(error ERROR: Incompatible options)
endif
ifeq ($(WITHRK16),)
  WWITHRK16      :=$(shell $(FFC) -o scripts/testreal128.exe scripts/testreal128.f90 &>comp.log ; ./scripts/testreal128.exe ; rm comp.log scripts/testreal128.exe)
else
  WWITHRK16      := $(WITHRK16)
endif
ifneq ($(WWITHRK16),$(filter $(WWITHRK16),0 1))
  $(info *********** WITHRK16 (compilation with real128):        $(WWITHRK16))
  $(info Possible values: 0, 1)
  $(error ERROR: Incompatible options)
endif
ifneq ($(INT),$(filter $(INT),4 8))
  $(info *********** INT (change default integer):        $(INT))
  $(info Possible values: 4, 8)
  $(error ERROR: Incompatible options)
endif
ifneq ($(RKIND),$(filter $(RKIND),real32 real64 real128))
  $(info *********** RKIND (select the real kind):        $(RKIND))
  $(info Possible values (case sensitive): real32 real64 real128)
  $(error ERROR: Incompatible options)
endif
#=================================================================================
ifeq ($(RKIND),real128)
  ifeq ($(WWITHRK16),0)
    $(info "Incompatible options:")
    $(info ***********RKIND:        $(RKIND))
    $(info ***********WITHRK16:     $(WWITHRK16))
    $(error ERROR: Incompatible options)
  endif
endif
#=================================================================================
#
# Operating system, OS? automatic using uname:
OS=$(shell uname)
#
# Extension for the object directory and the library
ext_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)_$(RKIND)
extold_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)

#
# library name
QDLIBA=lib$(QDLIB)$(ext_obj).a
QDLIBOLDA=lib$(QDLIB)$(extold_obj).a
#=================================================================================
#
OBJ_DIR=OBJ/obj$(ext_obj)
OBJOLD_DIR=OBJ/obj$(extold_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
#
MOD_DIR=$(OBJ_DIR)
EXTMod= 

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=TESTS
TESTSOUT_DIR=TESTS/output
#
QD_VERSION=$(shell awk '/version/ {print $$3}' fpm.toml | head -1)
#
CPPSHELL    = -D__COMPILE_DATE="\"$(shell date +"%a %e %b %Y - %H:%M:%S")\"" \
              -D__COMPILE_HOST="\"$(shell hostname -s)\"" \
              -D__QD_VERSION='$(QD_VERSION)' \
              -D__RKIND="$(RKIND)" -D__WITHRK16="$(WWITHRK16)" \
              -D__LAPACK="$(LLAPACK)"

#=================================================================================
# To deal with external compilers.mk file
CompilersDIR=
ifeq ($(CompilersDIR),)
  include compilers.mk
else
  include $(CompilersDIR)/compilers.mk
endif
#=================================================================================
#=================================================================================

$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(FFC))
$(info ***********COMPILER_VER: $(FC_VER))
$(info ***********OPTIMIZATION: $(OOPT))
$(info ***********OpenMP:       $(OOMP))
$(info ***********INT:          $(INT))
$(info ***********RKIND:        $(RKIND))
$(info ***********WITHRK16:     $(WWITHRK16))
$(info ***********LAPACK:       $(LLAPACK))
$(info ***********QD_VERSION:   $(QD_VERSION))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********************************************************************)
$(info ***********OBJ_DIR:      $(OBJ_DIR))
$(info ***********OBJOLD_DIR:   $(OBJOLD_DIR))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR) $(TESTS_DIR) $(SRC_DIR)  \
        $(SRC_DIR)/Test  \
        $(SRC_DIR)/NumParameters $(SRC_DIR)/String $(SRC_DIR)/File $(SRC_DIR)/Math $(SRC_DIR)/Quadrature \
        $(SRC_DIR)/Frac $(SRC_DIR)/File $(SRC_DIR)/Time \
        $(SRC_DIR)/Memory

QDLIB=QD

MAIN=App_QDLib
TESTS=Test_QDLib

# liste of source files in SRCFILES
include ./fortranlist.mk

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))

#===============================================
#============= all: lib, tests ...  ============
#===============================================
.PHONY: all
all: lib app ut
#===============================================
#============= Tests ===========================
#===============================================
.PHONY: help
help:
	@echo " Makefile usage:"
	@./scripts/makefile_usage.sh
#===============================================
#============= Tests ===========================
#===============================================
.PHONY: ut UT
UT ut: $(TESTS).x
	mkdir -p $(TESTSOUT_DIR)
	cd $(TESTSOUT_DIR) ; ../../$(TESTS).x > test.log
	@grep "Number of error(s)" $(TESTSOUT_DIR)/test.log
	@awk  -F: 'BEGIN{test=0} /Number of tests/ {test+=$$2} END {print "Number of tests: " test}' $(TESTSOUT_DIR)/test.log
	@awk  -F: 'BEGIN{err=0} /Number of error/ {err=err+$$2} END {print "Number of error(s) for all tests: " err}' $(TESTSOUT_DIR)/test.log
	@echo "  done Tests"
#===============================================
#============= Main executable and tests  ======
#===============================================
.PHONY: exe app
app exe: $(MAIN).x
$(MAIN).x: $(OBJ_DIR)/$(MAIN).o $(QDLIBA)
	$(FFC) $(FFLAGS) -o $(MAIN).x  $(OBJ_DIR)/$(MAIN).o  $(QDLIBA) $(FLIB)

$(TESTS).x: $(OBJ_DIR)/$(TESTS).o $(QDLIBA)
	$(FFC) $(FFLAGS) -o $(TESTS).x  $(OBJ_DIR)/$(TESTS).o $(QDLIBA) $(FLIB)
#===============================================
#============= Library: libQD.a  ===============
#===============================================
.PHONY: lib
lib: $(QDLIBA)

$(QDLIBA): $(OBJ)
	ar -cr $(QDLIBA) $(OBJ)
	cd OBJ ; ln -sf obj$(ext_obj) obj$(extold_obj)
	ln -sf  $(QDLIBA) $(QDLIBOLDA)
	@echo "  OBJ_DIR:      "$(OBJ_DIR)
	@echo "  OBJOLD_DIR:   "$(OBJOLD_DIR)
	@echo "  done Library: "$(QDLIBA)


# In the makefile, symling with directories does not work !!
#	ln -s $(OBJ_DIR) $(OBJOLD_DIR)
#	ln -s OBJ/obj$(ext_obj) OBJ/obj$(extold_obj)
#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FFC) $(FFLAGS) -o $@ -c $<
#===============================================
#============= documentation with ford =========
#===============================================
.PHONY: doc
doc:
	ford doc/ford-front-matter.md
#===============================================
#================ cleaning =====================
#===============================================
.PHONY: clean cleanall
clean:
	rm -f $(OBJ_DIR)/*.o
	rm -f SRC/*.mod SRC/*/*.mod
	rm -f *.log test*.txt file.*
	rm -f Test*.x App*.x
	@echo "  done cleaning"
#
cleanall : clean
	rm -fr OBJ/obj* OBJ/*mod build
	rm -fr doc/ford_site
	cd $(TESTS_DIR) ; ./clean
	rm -f libQD*.a
	@echo "  done all cleaning"
#
#===============================================
#============= make dependencies =============
#===============================================
.PHONY: dep
dependencies.mk fortranlist.mk dep:
	./scripts/dependency.sh
#===============================================
#============= module dependencies =============
#===============================================
$(OBJ_DIR)/$(MAIN).o:               $(QDLIBA)
$(OBJ_DIR)/$(TESTS).o:              $(QDLIBA)

$(QDLIBA): $(OBJ)
include ./dependencies.mk
