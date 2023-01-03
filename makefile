#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 19. linux)
#                gfortran (version: 9. linux and osx)
#                nagfor (version 7.0, osx)
 F90 = gfortran
#F90 = ifort
#F90 = nagfor
#
# Optimize? Empty: default No optimization; 0: No Optimization; 1 Optimzation
OPT = 1
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
#=================================================================================

#=================================================================================
# Operating system, OS? automatic using uname:
#=================================================================================
OS:=$(shell uname)

#=================================================================================
# extansion for the library (.a), objects and modules directory
#=================================================================================
ext_obj :=_$(F90)_opt$(OPT)_omp$(OMP)

#=================================================================================
# Directories
#=================================================================================
MAIN_path :=$(shell pwd)
OBJ_DIR    := $(MAIN_path)/OBJ/obj$(ext_obj)
$(shell [ -d $(OBJ_DIR) ] || mkdir -p $(OBJ_DIR))
MOD_DIR    := $(OBJ_DIR)

#=================================================================================
# External Libraries directory (dnSVM ...)
ifeq ($(ExternalLibDIR),)
  ExternalLibDIR := $(MAIN_path)/Ext_Lib
endif

QD_DIR=$(ExternalLibDIR)/QDUtilLib
QDMOD_DIR=$(QD_DIR)/OBJ/obj$(ext_obj)
QDLib=$(QD_DIR)/libQD$(ext_obj).a
#===============================================================================

#=================================================================================
# nag compillation (nagfor)
#=================================================================================
ifeq ($(F90),nagfor)
   # omp management
   ifeq ($(OMP),0)
      OMPFLAG =
   else
      OMPFLAG = -openmp
   endif
   # opt management
   ifeq ($(OPT),1)
      F90FLAGS = -O4  $(OMPFLAG) -Ounroll=4 -v
   else
      #F90FLAGS = -O0 $(OMPFLAG) -g -C=all -mtrace=all
      #  -C=undefined is not compatible with -framework Accelerate (because the routines are not compilled with the -C=undefined option)
      #with -mtrace=all add information on the memmory allocation/deallocation.
      ifeq ($(OMP),0)
        F90FLAGS =  -O0 $(OMPFLAG) -g -gline -C=all -C=undefined
      else
        F90FLAGS = -O0 $(OMPFLAG) -g -C=all
      endif
   endif

   ifeq ($(LAPACK),1)
     F90LIB = -framework Accelerate
   else
     F90LIB =
   endif

   F90_VER = $(shell $(F90) -V 3>&1 1>&2 2>&3 | head -1 )

endif
#=================================================================================
#=================================================================================
# ifort compillation v17 v18 with mkl
#=================================================================================
ifeq ($(F90),ifort)
   # omp management
   ifeq ($(OMP),0)
      OMPFLAG =
   else
      OMPFLAG = -qopenmp
   endif
   # opt management
   ifeq ($(OPT),1)
      F90FLAGS = -O  $(OMPFLAG) -parallel -g -traceback
   else
      F90FLAGS = -O0 $(OMPFLAG) -check all -g -traceback
   endif
   F90LIB = -qmkl -lpthread


   F90_VER = $(shell $(F90) --version | head -1 )

endif
#=================================================================================
#=================================================================================

#=================================================================================
# pgf90 compillation v12 with mkl
#=================================================================================
ifeq ($(F90),pgf90)
   # omp management
   ifeq ($(OMP),0)
      OMPFLAG =
   else
       OMPFLAG = -mp=allcores
       F90LIB = -lpthread
   endif
   # opt management
   ifeq ($(OPT),1)
      F90FLAGS = -O $(OMPFLAG) -fast -Mallocatable=03
   else
      F90FLAGS = -O0 $(OMPFLAG)      -Mallocatable=03 -Mbounds -Mchkstk -g
   endif

   ifeq ($(LAPACK),1)
     F90LIB += -lblas -llapack
   else
     F90LIB +=
   endif

   F90_VER = $(shell $(F90) --version | head -2 | tail -1 )

endif

#=================================================================================
#=================================================================================
# gfortran (osx and linux)
#=================================================================================
 ifeq ($(F90),gfortran)
   # omp management
   ifeq ($(OMP),0)
      OMPFLAG =
   else
      OMPFLAG = -fopenmp
   endif
   # OS management
   ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      F90LIB = -framework Accelerate
   else                   # Linux
      # linux libs
      F90LIB = -llapack -lblas
      #
      # linux libs with mkl and with openmp
      #F90LIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_gnu_thread
      # linux libs with mkl and without openmp
      #F90LIB = -L$(MKLROOT)/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential
   endif
   #
   # opt management
   ifeq ($(OPT),1)
      F90FLAGS = -O5 -g -fbacktrace $(OMPFLAG) -funroll-loops -ftree-vectorize -falign-loops=16
   else
      F90FLAGS = -Og -g -fbacktrace $(OMPFLAG) -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
      #F90FLAGS = -O0 -fbounds-check -Wuninitialized
   endif
   F90FLAGS += -I$(QDMOD_DIR)
   F90LIB   += $(QDLib)

   F90_VER = $(shell $(F90) --version | head -1 )

endif

#=================================================================================
#=================================================================================
$(info ***********************************************************************)
$(info ***********OS:           $(OS))
$(info ***********COMPILER:     $(F90))
$(info ***********COMPILER_VER: $(F90_VER))
$(info ***********OPTIMIZATION: $(OPT))
$(info ***********OpenMP:       $(OMPFLAG))
$(info ***********F90FLAGS:     $(F90FLAGS))
$(info ***********F90LIB:       $(F90LIB))
$(info ***********ext_obj:      $(ext_obj))
$(info ***********QD_DIR:        $(QD_DIR))
$(info ***********************************************************************)


F90_FLAGS = $(F90) $(F90FLAGS)
LYNK90    = $(F90_FLAGS)


 LIBS := $(PESLIB) $(F90LIB) $(ARPACKLIB) $(QDLIB)
 LYNKFLAGS = $(LIBS)


#
TEST_dnSEXE    = Test_dnS.x
TEST_dnPolyEXE = Test_dnPoly.x
EXA_dnSEXE     = Exa_dnS.x
LIBADshort     = libAD_dnSVM
LIBAD          = libAD_dnSVM$(ext_obj)

DIRAPP    = $(MAIN_path)/APP
DIRTEST   = $(MAIN_path)/Tests

DIRSRC    = $(MAIN_path)/SRC
DIRLib    = $(DIRSRC)/ADLib
DIRdnSVM  = $(DIRSRC)/ADdnSVM
#
OBJ_testdnS    = $(OBJ_DIR)/TEST_dnS.o
OBJ_testdnPoly = $(OBJ_DIR)/TEST_dnPoly.o


OBJ_lib        = $(OBJ_DIR)/dnSVM_m.o $(OBJ_DIR)/dnMat_m.o \
                 $(OBJ_DIR)/dnFunc_m.o $(OBJ_DIR)/dnPoly_m.o \
                 $(OBJ_DIR)/dnS_Op_m.o $(OBJ_DIR)/dnS_m.o \
                 $(OBJ_DIR)/UtilLib_m.o
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
	$(LYNK90)   -o $(EXA_dnSEXE) $(OBJ_DIR)/Example_dnS.o $(LIBAD).a $(LYNKFLAGS)
	echo "Exa_dnS compilation: OK"
$(OBJ_DIR)/Example_dnS.o:$(DIRAPP)/Example_dnS.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)  -c $(DIRAPP)/Example_dnS.f90

# Test dnS
.PHONY: dns dnS testdns testdnS
dns dnS testdns testdnS:$(TEST_dnSEXE)
	echo "dnS compilation: OK"
$(TEST_dnSEXE): $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a
	$(LYNK90)   -o $(TEST_dnSEXE) $(OBJ_DIR)/TEST_dnS.o $(LIBAD).a $(LYNKFLAGS)
	echo "dnS compilation: OK"
$(OBJ_DIR)/TEST_dnS.o:$(DIRTEST)/TEST_dnS.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)  -c $(DIRTEST)/TEST_dnS.f90

# Test dnPoly
.PHONY: dnpoly dnPoly testdnpoly testdnPoly
dnpoly dnPoly testdnpoly testdnPoly: $(TEST_dnPolyEXE)
	echo "OBJ_testdnPoly compilation: OK"
$(TEST_dnPolyEXE): $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a
	$(LYNK90)   -o $(TEST_dnPolyEXE) $(OBJ_DIR)/TEST_dnPoly.o $(LIBAD).a $(LYNKFLAGS)
	echo "dnPoly compilation: OK"
$(OBJ_DIR)/TEST_dnPoly.o:$(DIRTEST)/TEST_dnPoly.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)  -c $(DIRTEST)/TEST_dnPoly.f90
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
#============= Library: libAD_dnSVM.a  =========
#===============================================
.PHONY: lib libad libAD
lib libad libAD: $(LIBAD).a
$(LIBAD).a: $(OBJ_lib)
	ar -cr $(LIBAD).a $(OBJ_lib)
	rm -f $(LIBADshort).a
	ln -s  $(LIBAD).a $(LIBADshort).a
	@echo "  done Library: "$(LIBAD).a
#===============================================
#===============================================

#===============================================
#================ cleaning =====================
.PHONY: clean
clean:
	rm -f  $(TEST_dnSEXE) $(TEST_dnPolyEXE) $(EXA_dnSEXE) $(LIBAD).a
	rm -f  dnSca.txt comp.log dnS.log dnPoly.log Test.log
	rm -fr *.dSYM
	rm -fr build
	cd $(OBJ_DIR) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning up the example directories"
.PHONY: cleanall
cleanall: clean
	rm -f *.a
	rm -rf OBJ
	cd $(MAIN_path)/Ext_Lib ; ./cleanlib
	@echo "  done remove *.a libraries and OBJ directory"
#===============================================
#===============================================
#
##################################################################################
### dnSVM objects
#
$(OBJ_DIR)/dnSVM_m.o:$(DIRSRC)/dnSVM_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)  -c $(DIRSRC)/dnSVM_m.f90
	@echo "make dnSVM_m.o"
#
### dnS, dnPoly, dnMat objects
#
$(OBJ_DIR)/dnS_m.o:$(DIRdnSVM)/dnS_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)  -c $(DIRdnSVM)/dnS_m.f90
$(OBJ_DIR)/dnPoly_m.o:$(DIRdnSVM)/dnPoly_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS) -c $(DIRdnSVM)/dnPoly_m.f90
$(OBJ_DIR)/dnFunc_m.o:$(DIRdnSVM)/dnFunc_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS) -c $(DIRdnSVM)/dnFunc_m.f90
$(OBJ_DIR)/dnS_Op_m.o:$(DIRdnSVM)/dnS_Op_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS) -c $(DIRdnSVM)/dnS_Op_m.f90
$(OBJ_DIR)/dnMat_m.o:$(DIRdnSVM)/dnMat_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)   -c $(DIRdnSVM)/dnMat_m.f90
##################################################################################
#
#
##################################################################################
### libraries
#
$(OBJ_DIR)/UtilLib_m.o:$(DIRLib)/UtilLib_m.f90
	cd $(OBJ_DIR) ; $(F90_FLAGS)   -c $(DIRLib)/UtilLib_m.f90
#
##################################################################################
#
##################################################################################
### external libraries
#
$(ExternalLibDIR):
	@echo directory $(ExternalLibDIR) does not exist
	exit 1

$(QDLib): $(ExternalLibDIR)
	cd $(ExternalLibDIR) ; ./get_QDUtilLib.sh
	@echo "  done QDUtilLib"
#
##################################################################################

#
##################################################################################
### dependencies
#
$(OBJ_DIR)/Example_dnS.o:      $(LIBAD).a
$(OBJ_DIR)/TEST_dnS.o:         $(LIBAD).a
$(OBJ_DIR)/TEST_dnPoly.o:      $(LIBAD).a
$(LIBAD).a:                   $(OBJ_lib)
#
$(OBJ_DIR)/dnSVM_m.o:          $(OBJ_DIR)/dnS_m.o $(OBJ_DIR)/dnPoly_m.o \
                              $(OBJ_DIR)/dnFunc_m.o $(OBJ_DIR)/dnS_Op_m.o \
                              $(OBJ_DIR)/dnMat_m.o
#
$(OBJ_DIR)/dnPoly_m.o:         $(OBJ_DIR)/dnS_m.o $(OBJ_DIR)/UtilLib_m.o
$(OBJ_DIR)/dnFunc_m.o:         $(OBJ_DIR)/dnPoly_m.o $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnMat_m.o:          $(OBJ_DIR)/dnS_m.o
$(OBJ_DIR)/dnS_m.o:            $(OBJ_DIR)/UtilLib_m.o
#
$(OBJ_DIR)/UtilLib_m.o:        $(QDLib)
#
############################################################################