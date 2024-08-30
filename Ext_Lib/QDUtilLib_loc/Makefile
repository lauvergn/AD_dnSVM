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

#=================================================================================
#
# Operating system, OS? automatic using uname:
OS=$(shell uname)
#
# Extension for the object directory and the library
ext_obj=_$(FFC)_opt$(OOPT)_omp$(OOMP)_lapack$(LLAPACK)_int$(INT)
#
# library name
QDLIBA=lib$(QDLIB)$(ext_obj).a
#=================================================================================
#
OBJ_DIR=OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
#
MOD_DIR=$(OBJ_DIR)
EXTMod= 

SRC_DIR=SRC
MAIN_DIR=APP
TESTS_DIR=TESTS
#
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
$(info ***********LAPACK:       $(LLAPACK))
$(info ***********FFLAGS:       $(FFLAGS))
$(info ***********FLIB:         $(FLIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********************************************************************)


VPATH = $(MAIN_DIR) $(TESTS_DIR) $(SRC_DIR)  \
        $(SRC_DIR)/Test  \
        $(SRC_DIR)/NumParameters $(SRC_DIR)/String $(SRC_DIR)/File $(SRC_DIR)/Math \
        $(SRC_DIR)/Frac $(SRC_DIR)/File $(SRC_DIR)/Time \
        $(SRC_DIR)/Memory

QDLIB=QD

MAIN=App_QDLib
TESTS=Test_QDLib

SRCFILES=Test_m.f90 NumParameters_m.f90 MathUtil_m.f90 FFT_m.f90 \
         String_m.f90 RW_MatVec_m.f90 Matrix_m.f90 Vector_m.f90 Diago_m.f90 \
         IntVec_m.f90 RealVec_m.f90 \
         Frac_m.f90 File_m.f90 Time_m.f90 \
         Memory_NotPointer_m.f90 Memory_Pointer_m.f90 Memory_base_m.f90 Memory_m.f90 \
         QDUtil_m.f90

OBJ0=${SRCFILES:.f90=.o}
OBJ=$(addprefix $(OBJ_DIR)/, $(OBJ0))

#===============================================
#============= Tests ===========================
#===============================================
.PHONY: ut UT
UT ut: $(TESTS).x
	./$(TESTS).x > test.log
	grep "Number of error(s)" test.log
	@echo "  done Tests"


#===============================================
#============= all: lib, tests ...  ============
#===============================================
.PHONY: all
all: $(QDLIBA) $(MAIN).x $(TESTS).x
#===============================================
#============= Main executable and tests  ======
#===============================================
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
	@echo "  done Library: "$(QDLIBA)

#===============================================
#============= compilation =====================
#===============================================
$(OBJ_DIR)/%.o: %.f90
	$(FFC) $(FFLAGS) -o $@ -c $<
#===============================================
#================ cleaning =====================
.PHONY: clean cleanall
clean:
	rm -f $(OBJ_DIR)/*/*.o $(OBJ_DIR)/*.o
	rm -f *.log test*.txt file.*
	rm -f Test*.x App*.x
	@echo "  done cleaning"
#
cleanall : clean
	rm -fr OBJ/obj* OBJ/*mod build
	rm -f libQD*.a
	rm -f TESTS/res* TESTS/*log
	@echo "  done all cleaning"
#===============================================
#================ zip and copy the directory ===
ExtLibSAVEDIR := /Users/lauvergn/git/Ext_Lib
BaseName := QDUtilLib
.PHONY: zip
zip: cleanall
	test -d $(ExtLibSAVEDIR) || (echo $(ExtLibDIR) "does not exist" ; exit 1)
	$(ExtLibSAVEDIR)/makezip.sh $(BaseName)
	cd $(ExtLibSAVEDIR) ; ./cp_QDUtil.sh
	@echo "  done zip"
#===============================================
#============= module dependencies =============
#===============================================
$(OBJ_DIR)/NumParameters_m.o:       $(OBJ_DIR)/Test_m.o

$(OBJ_DIR)/Memory_base_m.o:         $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Memory_NotPointer_m.o:   $(OBJ_DIR)/Memory_base_m.o $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Memory_Pointer_m.o:      $(OBJ_DIR)/Memory_base_m.o $(OBJ_DIR)/NumParameters_m.o

$(OBJ_DIR)/Time_m.o:                $(OBJ_DIR)/NumParameters_m.o


$(OBJ_DIR)/MathUtil_m.o:            $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/FFT_m.o:                 $(OBJ_DIR)/String_m.o $(OBJ_DIR)/NumParameters_m.o

$(OBJ_DIR)/String_m.o:              $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_base_m.o

$(OBJ_DIR)/Frac_m.o:                $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o

$(OBJ_DIR)/File_m.o:                $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o

$(OBJ_DIR)/RW_MatVec_m.o:           $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/Matrix_m.o:              $(OBJ_DIR)/RW_MatVec_m.o
$(OBJ_DIR)/Vector_m.o:              $(OBJ_DIR)/RW_MatVec_m.o
$(OBJ_DIR)/Diago_m.o:               $(OBJ_DIR)/Matrix_m.o $(OBJ_DIR)/RW_MatVec_m.o

$(OBJ_DIR)/IntVec_m.o:              $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_m.o
$(OBJ_DIR)/RealVec_m.o:             $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_m.o


$(OBJ_DIR)/Memory_m.o:              $(OBJ_DIR)/Memory_NotPointer_m.o $(OBJ_DIR)/Memory_Pointer_m.o \
                                    $(OBJ_DIR)/String_m.o $(OBJ_DIR)/NumParameters_m.o

$(OBJ_DIR)/QDUtil_m.o:              $(OBJ_DIR)/Diago_m.o $(OBJ_DIR)/Matrix_m.o $(OBJ_DIR)/RW_MatVec_m.o \
                                    $(OBJ_DIR)/Frac_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/Time_m.o \
                                    $(OBJ_DIR)/Memory_m.o $(OBJ_DIR)/File_m.o

$(OBJ_DIR)/$(MAIN).o:               $(QDLIBA)
$(OBJ_DIR)/$(TESTS).o:              $(QDLIBA)
