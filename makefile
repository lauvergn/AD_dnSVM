#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 19. linux)
#                gfortran (version: 9. linux and osx)
#                nagfor (version 7.0, osx)
 FC = gfortran
#FC = ifort
#FC = nagfor
#
# Optimize? Empty: default Optimization; 0: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
## how to get external libraries;  "loc" (default): from local zip file, Empty or something else (v0.5): from github
EXTLIB_TYPE = loc
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
ifeq ($(OMP),)
  OOMP      := 1
else
  OOMP      := $(OMP)
endif
ifeq ($(LAPACK),)
  LLAPACK      := 1
else
  LLAPACK      := $(LAPACK)
endif
#=================================================================================
# Operating system, OS? automatic using uname:
#=================================================================================
OS:=$(shell uname)

#=================================================================================
# extansion for the library (.a), objects and modules directory
#=================================================================================
ext_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)

#=================================================================================
# Directories
#=================================================================================
MAIN_path :=$(shell pwd)

OBJ_DIR    := OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR    := $(OBJ_DIR)

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=Tests

#=================================================================================
# External Libraries directory
ifeq ($(ExtLibDIR),)
  ExtLibDIR := Ext_Lib
endif

QD_DIR=$(ExtLibDIR)/QDUtilLib
QDMOD_DIR=$(QD_DIR)/OBJ/obj$(ext_obj)
QDLIBA=$(QD_DIR)/libQD$(ext_obj).a
#===============================================================================

#=================================================================================
#=================================================================================
# gfortran (osx and linux)
#=================================================================================
ifeq ($(FFC),gfortran)

  ifeq ($(OOPT),1)
    FFLAGS = -O5 -g -fbacktrace -funroll-loops -ftree-vectorize -falign-loops=16
  else
    FFLAGS = -Og -g -fbacktrace -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
    #FFLAGS = -O0 -fbounds-check -Wuninitialized
  endif

  # where to store .mod files
  FFLAGS +=-J$(MOD_DIR)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -fopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # where to look .mod files
  FFLAGS += -I$(QDMOD_DIR)


   FLIB   = $(QDLIBA)
  # OS management
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB += -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB += -llapack -lblas
      #
      # linux libs with mkl and with openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread
      # linux libs with mkl and without openmp
      #FLIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential
    endif
  endif


   FC_VER = $(shell $(FFC) --version | head -1 )

endif
#=================================================================================
#=================================================================================
$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(FFC))
$(info ***********COMPILER_VER: $(FC_VER))
$(info ***********OPTIMIZATION: $(OOPT))
$(info ***********OpenMP:       $(OOMP))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********QD_DIR:       $(QD_DIR))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR):$(TESTS_DIR):$(SRC_DIR):$(SRC_DIR)/ADdnSVM

SRCFILES=dnSVM_m.f90 dnMat_m.f90 dnFunc_m.f90 dnPoly_m.f90 dnS_Op_m.f90 dnS_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))


#
TEST_dnSEXE    = Test_dnS.x
TEST_dnPolyEXE = Test_dnPoly.x
EXA_dnSEXE     = Exa_dnS.x
LIBADshort     = libAD_dnSVM
LIBAD          = libAD_dnSVM$(ext_obj)
#===============================================
#============= Main programs: tests + example ==
#
.PHONY: all
all: dnS dnPoly exa

# Example dnS
.PHONY: exa exa_dnS Exa_dnS
exa exa_dnS Exa_dnS:$(EXA_dnSEXE)
	echo "Exa_dnS compilation: OK"
$(EXA_dnSEXE): $(OBJ_DIR)/Example_dnS.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(EXA_dnSEXE) $(OBJ_DIR)/Example_dnS.o $(LIBAD).a $(FLIB)
	echo "Exa_dnS compilation: OK"
# Test dnS
.PHONY: dns dnS testdns testdnS
dns dnS testdns testdnS:$(TEST_dnSEXE)
	echo "dnS compilation: OK"
$(TEST_dnSEXE): $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnSEXE) $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a $(FLIB)
	echo "dnS compilation: OK"
# Test dnPoly
.PHONY: dnpoly dnPoly testdnpoly testdnPoly
dnpoly dnPoly testdnpoly testdnPoly: $(TEST_dnPolyEXE)
	echo "OBJ_testdnPoly compilation: OK"
$(TEST_dnPolyEXE): $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnPolyEXE) $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a $(FLIB)
	echo "dnPoly compilation: OK"
#
#===============================================
#================ unitary tests ================
.PHONY: ut UT
ut UT: $(TEST_dnPolyEXE) $(TEST_dnSEXE)
	@./$(TEST_dnSEXE) > Test.log
	@./$(TEST_dnPolyEXE) >> Test.log
	@grep -F TESTING Test.log| grep -F Number
	@echo "  done with the unitary tests"
#===============================================
#===============================================
#
#===============================================
#============= compilation dnSVM ===============
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FFC) $(FFLAGS) -o $@ -c $<
#
#===============================================
#============= Library: libAD_dnSVM.a  =========
#===============================================
.PHONY: lib libad libAD
lib libad libAD: $(LIBAD).a
$(LIBAD).a: $(OBJ)
	ar -cr $(LIBAD).a $(OBJ)
	rm -f $(LIBADshort).a
	ln -s  $(LIBAD).a $(LIBADshort).a
	@echo "  done Library: "$(LIBAD).a
#===============================================
#===============================================
#
#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f  $(TEST_dnSEXE) $(TEST_dnPolyEXE) $(EXA_dnSEXE) $(LIBAD).a
	rm -f  dnSca.txt comp.log dnS.log dnPoly.log Test.log
	rm -fr *.dSYM
	rm -fr build
	cd $(OBJ_DIR) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning up the example directories"
cleanall: clean
	rm -f *.a
	rm -rf OBJ
	cd $(MAIN_path)/Ext_Lib ; ./cleanlib
	@echo "  done remove *.a libraries and OBJ directory"
#===============================================
#================ zip and copy the directory ===
ExtLibSAVEDIR := /Users/lauvergn/git/Ext_Lib
BaseName := AD_dnSVM
.PHONY: zip
zip: cleanall
	test -d $(ExtLibSAVEDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	cd $(ExtLibSAVEDIR) ; rm -rf $(BaseName)_devloc
	mkdir $(ExtLibSAVEDIR)/$(BaseName)_devloc
	cp -r * $(ExtLibSAVEDIR)/$(BaseName)_devloc
	cd $(ExtLibSAVEDIR) ; zip -r Save_$(BaseName)_devloc.zip $(BaseName)_devloc
	cd $(ExtLibSAVEDIR) ; rm -rf $(BaseName)_devloc
	@echo "  done zip"
#===============================================
#===============================================
#== external libraries
#
$(QDLIBA):
	@test -d $(ExtLibDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	@test -d $(QD_DIR) || (cd $(ExtLibDIR) ; ./get_QDUtilLib.sh $(EXTLIB_TYPE))
	@test -d $(QD_DIR) || (echo $(QD_DIR) "does not exist" ; exit 1)
	cd $(QD_DIR) ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) ExtLibDIR=$(ExtLibDIR)
	@echo "  done " $(QDLIBA) " in "$(BaseName)
#
#===============================================


#
#===============================================
### dependencies
#
$(OBJ_DIR)/Example_dnS.o:      $(LIBAD).a
$(OBJ_DIR)/TEST_dnS.o:         $(LIBAD).a
$(OBJ_DIR)/TEST_dnPoly.o:      $(LIBAD).a
$(LIBAD).a:                    $(OBJ_lib)
#
$(OBJ_DIR)/dnSVM_m.o:          $(OBJ_DIR)/dnS_m.o $(OBJ_DIR)/dnPoly_m.o \
                               $(OBJ_DIR)/dnFunc_m.o $(OBJ_DIR)/dnS_Op_m.o \
                               $(OBJ_DIR)/dnMat_m.o
#
$(OBJ_DIR)/dnPoly_m.o:         $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnFunc_m.o:         $(OBJ_DIR)/dnPoly_m.o $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnMat_m.o:          $(OBJ_DIR)/dnS_m.o
#
$(OBJ_DIR)/dnS_m.o:            $(QDLIBA)
#
#===============================================




#=================================================================================
#=================================================================================
# ifort compillation v17 v18 with mkl
#=================================================================================
ifeq ($(FFC),ifort)

  # opt management
  ifeq ($(OOPT),1)
      #F90FLAGS = -O -parallel -g -traceback
      FFLAGS = -O  -g -traceback
  else
      FFLAGS = -O0 -check all -g -traceback
  endif

  # where to store the modules
  FFLAGS +=-module $(MOD_DIR)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -qopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # where to look .mod files
  FFLAGS += -I$(QDMOD_DIR)

  FLIB    = $(QDLIBA)
  ifeq ($(LLAPACK),1)
    FLIB += -mkl -lpthread
  else
    FLIB += -lpthread
  endif

  FC_VER = $(shell $(F90) --version | head -1 )

endif
#=================================================================================
#=================================================================================