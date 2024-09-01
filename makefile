#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 19. linux) or ifx
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
## force the default integer (without kind) during the compillation.
## default 4: , INT=8 (for kind=8)
INT = 4
## how to get external libraries;  "loc" (default): from local zip file, Empty or something else, from github
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
ext_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)

#=================================================================================
# Directories
#=================================================================================
MAIN_path := $(shell pwd)

OBJ_DIR    := OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR    := $(OBJ_DIR)

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=Tests
#
#=================================================================================
# To deal with external compilers.mk file
CompilersDIR = $(MAIN_path)
ifeq ($(CompilersDIR),)
  include compilers.mk
else
  include $(CompilersDIR)/compilers.mk
endif
#=================================================================================
# External Libraries directory
ifeq ($(ExtLibDIR),)
  ExtLibDIR := $(MAIN_path)/Ext_Lib
endif
$(shell [ -d $(ExtLibDIR) ] || (echo $(ExtLibDIR) "does not exist" ; exit 1))
#
QD_DIR=$(ExtLibDIR)/QDUtilLib
QDMOD_DIR=$(QD_DIR)/OBJ/obj$(ext_obj)
QDLIBA=$(QD_DIR)/libQD$(ext_obj).a
#===============================================================================

EXTLib     = $(QDLIBA)
EXTMod     = -I$(QDMOD_DIR)
#=================================================================================
#=================================================================================
#=================================================================================
$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(FFC))
$(info ***********COMPILER_VER: $(FC_VER))
$(info ***********OPTIMIZATION: $(OOPT))
$(info ***********OpenMP:       $(OOMP))
$(info ***********INT:          $(INT))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********QD_DIR:       $(QD_DIR))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR):$(TESTS_DIR):$(SRC_DIR):$(SRC_DIR)/ADdnSVM

SRCFILES=dnSVM_m.f90 dnMat_m.f90 dnVec_m.f90 dnFunc_m.f90 dnPoly_m.f90 dnS_Op_m.f90 dnS_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))


#
TEST_dnSEXE    = Test_dnS.x
TEST_dnPolyEXE = Test_dnPoly.x
TEST_dnVecEXE  = Test_dnVec.x
TEST_dnMatEXE  = Test_dnMat.x

EXA_dnSEXE     = Exa_dnS.x
LIBADshort     = libAD_dnSVM
LIBAD          = libAD_dnSVM$(ext_obj)
#===============================================
#============= Main programs: tests + example ==
#
.PHONY: all
all: dnS dnPoly dnV dnM exa

# Example dnS
.PHONY: exa exa_dnS Exa_dnS
exa exa_dnS Exa_dnS:$(EXA_dnSEXE)
	echo "Exa_dnS compilation: OK"
$(EXA_dnSEXE): $(OBJ_DIR)/Example_dnS.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(EXA_dnSEXE) $(OBJ_DIR)/Example_dnS.o $(LIBAD).a $(EXTLib) $(FLIB)
	echo "Exa_dnS compilation: OK"
# Test dnS
.PHONY: dns dnS testdns testdnS
dns dnS testdns testdnS:$(TEST_dnSEXE)
	echo "dnS compilation: OK"
$(TEST_dnSEXE): $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnSEXE) $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a $(EXTLib) $(FLIB)
	echo "dnS compilation: OK"
# Test dnPoly
.PHONY: dnpoly dnPoly testdnpoly testdnPoly
dnpoly dnPoly testdnpoly testdnPoly: $(TEST_dnPolyEXE)
	echo "OBJ_testdnPoly compilation: OK"
$(TEST_dnPolyEXE): $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnPolyEXE) $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a $(EXTLib) $(FLIB)
	echo "dnPoly compilation: OK"
# Test dnVec
.PHONY: dnV dnv tesdnVec tesdnV tesdnv
dnV dnv tesdnVec tesdnV tesdnv:$(TEST_dnVecEXE)
	echo "dnVec compilation: OK"
$(TEST_dnVecEXE): $(OBJ_DIR)/TEST_dnVec.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnVecEXE) $(OBJ_DIR)/TEST_dnVec.o $(LIBAD).a $(EXTLib) $(FLIB)
	echo "dnVec compilation: OK"
#
# Test dnMat
.PHONY: dnM
dnM:$(TEST_dnMatEXE)
	echo "dnVec compilation: OK"
$(TEST_dnMatEXE): $(OBJ_DIR)/TEST_dnMat.o $(LIBAD).a
	$(FFC) $(FFLAGS)   -o $(TEST_dnMatEXE) $(OBJ_DIR)/TEST_dnMat.o $(LIBAD).a $(EXTLib) $(FLIB)
	echo "dnVec compilation: OK"
#
#===============================================
#================ unitary tests ================
.PHONY: ut UT
ut UT: $(TEST_dnPolyEXE) $(TEST_dnSEXE) $(TEST_dnVecEXE) $(TEST_dnMatEXE)
	@./$(TEST_dnSEXE) > Test.log
	@./$(TEST_dnPolyEXE) >> Test.log
	@./$(TEST_dnVecEXE) >> Test.log
	@./$(TEST_dnMatEXE) >> Test.log
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
	rm -f  $(TEST_dnSEXE) $(TEST_dnPolyEXE) $(TEST_dnVecEXE) $(TEST_dnMatEXE) $(EXA_dnSEXE)
	rm -f  $(LIBAD).a
	rm -f  dnSca.txt *.log
	rm -fr *.dSYM
	rm -fr build
	cd $(OBJ_DIR) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning"
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
	$(ExtLibSAVEDIR)/makezip.sh $(BaseName)
	cd $(ExtLibSAVEDIR) ; ./cp_AD_dnSVM.sh
	@echo "  done zip"
#===============================================
#===============================================
#== external libraries
#
.PHONY: getlib
getlib:
	cd $(ExtLibDIR) ; ./get_Lib.sh QDUtilLib
$(QDLIBA):
	cd $(ExtLibDIR) ; ./get_Lib.sh QDUtilLib
	cd $(ExtLibDIR)/QDUtilLib ; make lib FC=$(FFC) OPT=$(OOPT) OMP=$(OOMP) LAPACK=$(LLAPACK) INT=$(INT) ExtLibDIR=$(ExtLibDIR) CompilersDIR=$(CompilersDIR)
	@test -f $(QDLIBA) || (echo $(QDLIBA) "does not exist" ; exit 1)
	@echo "  done " $(QDLIBA)
#
#===============================================


#
#===============================================
### dependencies
#
$(OBJ_DIR)/Example_dnS.o:      $(LIBAD).a
$(OBJ_DIR)/TEST_dnS.o:         $(LIBAD).a
$(OBJ_DIR)/TEST_dnPoly.o:      $(LIBAD).a
$(OBJ_DIR)/TEST_dnVec.o:       $(LIBAD).a
$(LIBAD).a:                    $(OBJ_lib)
#
$(OBJ_DIR)/dnSVM_m.o:          $(OBJ_DIR)/dnS_m.o $(OBJ_DIR)/dnPoly_m.o \
                               $(OBJ_DIR)/dnFunc_m.o $(OBJ_DIR)/dnS_Op_m.o \
                               $(OBJ_DIR)/dnVec_m.o $(OBJ_DIR)/dnMat_m.o
#
$(OBJ_DIR)/dnPoly_m.o:         $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnFunc_m.o:         $(OBJ_DIR)/dnPoly_m.o $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnMat_m.o:          $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnVec_m.o:          $(OBJ_DIR)/dnS_m.o
#
$(OBJ_DIR)/dnS_m.o:            | $(QDLIBA)
#
#===============================================