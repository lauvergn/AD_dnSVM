#=================================================================================
#=================================================================================
# Compiler?
#Possible values: (Empty: gfortran)
#                ifort (version: 14.0.2, 16.0.3, 17.0.1 linux)
#                gfortran (version: 6.3.0 linux and osx)
#                pgf90 (version: 17.10-0, linux)
#                nagfor (version 7.0, osx)
 F90 = gfortran
#F90 = nagfor
#
# Optimize? Empty: default No optimization; 0: No Optimization; 1 Optimzation
OPT = 0
## OpenMP? Empty: default with OpenMP; 0: No OpenMP; 1 with OpenMP
OMP = 1
## Lapack/blas/mkl? Empty: default with Lapack; 0: without Lapack; 1 with Lapack
LAPACK = 1
#=================================================================================

#=================================================================================
# If ExternalF90 is empty, F90 is unchanged
ifeq  ($(strip $(ExternalF90)),)
else
  F90 = $(ExternalF90)
endif
# If F90 is empty, F90=gfortran
ifeq  ($(strip $(F90)),)
  F90 = gfortran
endif
# If ExternalOPT is empty, OPT is unchanged
ifeq  ($(strip $(ExternalOPT)),)
else
  OPT = $(ExternalOPT)
endif
# If OPT is empty, OPT=0
ifeq  ($(strip $(OPT)),)
  OPT = 0
endif
# If ExternalOMP is empty, OMP is unchanged
ifeq  ($(strip $(ExternalOMP)),)
else
  OMP = $(ExternalOMP)
endif
# If OMP is empty, OMP=1
ifeq  ($(strip $(OMP)),)
  OMP = 1
endif
#=================================================================================

# Operating system, OS? automatic using uname:
OS=$(shell uname)


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
   F90LIB = -mkl -lpthread

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
$(info ***********************************************************************)


F90_FLAGS = $(F90) $(F90FLAGS)
LYNK90 = $(F90_FLAGS)

 LIBS := $(PESLIB) $(F90LIB) $(ARPACKLIB)
 LYNKFLAGS = $(LIBS)


#
TEST_dnSEXE    = Test_dnS.x
TEST_dnPolyEXE = Test_dnPoly.x
EXA_dnSEXE     = Exa_dnS.x
LIBAD          = libAD_dnSVM

DIR0     := $(shell pwd)
DIRAPP    = $(DIR0)/APP
DIRTEST   = $(DIR0)/Tests
DIROBJ    = $(DIR0)/OBJ
DIRSRC    = $(DIR0)/SRC
DIRLib    = $(DIRSRC)/ADLib
DIRdnSVM  = $(DIRSRC)/ADdnSVM
#
OBJ_testdnS    = $(DIROBJ)/TEST_dnS.o
OBJ_testdnPoly = $(DIROBJ)/TEST_dnPoly.o


OBJ_lib        = $(DIROBJ)/dnSVM_m.o $(DIROBJ)/dnMat_m.o $(DIROBJ)/dnPoly_m.o $(DIROBJ)/dnS_m.o \
                 $(DIROBJ)/UtilLib_m.o $(DIROBJ)/diago_m.o \
                 $(DIROBJ)/Test_m.o \
                 $(DIROBJ)/NumParameters_m.o

OBJ_all        = $(OBJ_lib)

#===============================================
#============= Main programs: tests + example ==
#
.PHONY: all
all: dnS dnPoly exa

# Example dnS
.PHONY: exa exa_dnS Exa_dnS
exa exa_dnS Exa_dnS:$(EXA_dnSEXE)
	echo "Exa_dnS compilation: OK"
$(EXA_dnSEXE): $(DIROBJ)/Example_dnS.o $(LIBAD).a
	$(LYNK90)   -o $(EXA_dnSEXE) $(DIROBJ)/Example_dnS.o $(LIBAD).a $(LYNKFLAGS)
	echo "Exa_dnS compilation: OK"
$(DIROBJ)/Example_dnS.o:$(DIRAPP)/Example_dnS.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRAPP)/Example_dnS.f90

# Test dnS
.PHONY: dns dnS testdns testdnS
dns dnS testdns testdnS:$(TEST_dnSEXE)
	echo "dnS compilation: OK"
$(TEST_dnSEXE): $(DIROBJ)/TEST_dnS.o $(LIBAD).a
	$(LYNK90)   -o $(TEST_dnSEXE) $(DIROBJ)/TEST_dnS.o $(LIBAD).a $(LYNKFLAGS)
	echo "dnS compilation: OK"
$(DIROBJ)/TEST_dnS.o:$(DIRTEST)/TEST_dnS.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRTEST)/TEST_dnS.f90

# Test dnPoly
.PHONY: dnpoly dnPoly testdnpoly testdnPoly
dnpoly dnPoly testdnpoly testdnPoly: $(TEST_dnPolyEXE)
	echo "OBJ_testdnPoly compilation: OK"
$(TEST_dnPolyEXE): $(DIROBJ)/TEST_dnPoly.o $(LIBAD).a
	$(LYNK90)   -o $(TEST_dnPolyEXE) $(DIROBJ)/TEST_dnPoly.o $(LIBAD).a $(LYNKFLAGS)
	echo "dnPoly compilation: OK"
$(DIROBJ)/TEST_dnPoly.o:$(DIRTEST)/TEST_dnPoly.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRTEST)/TEST_dnPoly.f90
#
#===============================================
#================ unitary tests ================
.PHONY: ut UT
ut UT: $(TEST_dnPolyEXE) $(TEST_dnSEXE)
	./$(TEST_dnSEXE)
	./$(TEST_dnPolyEXE)
	@echo "  done with the unitary tests"
#===============================================
#===============================================
#
#===============================================
#============= Library: libAD_dnSVM.a  =========
#===============================================
.PHONY: lib libad libAD
lib libad libAD: $(LIBAD).a
	@echo "  done Library: libAD_dnSVM.a"
$(LIBAD).a: $(OBJ_lib)
	ar -cr $(LIBAD).a $(OBJ_lib)
	@echo "  done Library: libAD_dnSVM.a"
#===============================================
#===============================================

#===============================================
#================ cleaning =====================
.PHONY: clean
clean:
	rm -f  $(TEST_dnSEXE) $(TEST_dnPolyEXE) $(EXA_dnSEXE) $(LIBAD).a
	rm -f  dnSca.txt comp.log dnS.log dnPoly.log
	rm -fr *.dSYM
	rm -fr build
	cd $(DIROBJ) ; rm -f *.o *.mod *.MOD
	@cd Tests && ./clean
	@echo "  done cleaning up the example directories"
#===============================================
#===============================================
#
#===============================================
#================ create OBJ directory =========
.PHONY: obj
obj:
	@echo "=> obj directory: $(obj_dir)"
	@mkdir -p $(DIROBJ)
#===============================================
#===============================================
#
#
##################################################################################
### dnSVM objects
#
$(DIROBJ)/dnSVM_m.o:$(DIRSRC)/dnSVM_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRSRC)/dnSVM_m.f90
#
### dnS, dnPoly, dnMat objects
#
$(DIROBJ)/dnS_m.o:$(DIRdnSVM)/dnS_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRdnSVM)/dnS_m.f90
$(DIROBJ)/dnPoly_m.o:$(DIRdnSVM)/dnPoly_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS) -c $(DIRdnSVM)/dnPoly_m.f90
$(DIROBJ)/dnMat_m.o:$(DIRdnSVM)/dnMat_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)   -c $(DIRdnSVM)/dnMat_m.f90
##################################################################################
#
#
##################################################################################
### libraries
#
$(DIROBJ)/UtilLib_m.o:$(DIRLib)/UtilLib_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)   -c $(DIRLib)/UtilLib_m.f90
$(DIROBJ)/Test_m.o:$(DIRLib)/Test_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)   -c $(DIRLib)/Test_m.f90
$(DIROBJ)/diago_m.o:$(DIRLib)/diago_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)  -c $(DIRLib)/diago_m.f90
$(DIROBJ)/NumParameters_m.o:$(DIRLib)/NumParameters_m.f90
	cd $(DIROBJ) ; $(F90_FLAGS)   -c $(DIRLib)/NumParameters_m.f90
#
##################################################################################
#
#
##################################################################################
### dependencies
#
$(DIROBJ)/Example_dnS.o: $(LIBAD).a
$(DIROBJ)/TEST_dnS.o:    $(LIBAD).a
$(DIROBJ)/TEST_dnPoly.o: $(LIBAD).a
$(LIBAD).a:              $(OBJ_lib)
#
$(DIROBJ)/dnSVM_m.o:      $(DIROBJ)/dnS_m.o $(DIROBJ)/dnPoly_m.o $(DIROBJ)/dnMat_m.o
#
$(DIROBJ)/UtilLib_m.o:    $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/Test_m.o:       $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/dnS_m.o:        $(DIROBJ)/UtilLib_m.o $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/dnPoly_m.o:     $(DIROBJ)/dnS_m.o $(DIROBJ)/UtilLib_m.o $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/dnMat_m.o:      $(DIROBJ)/dnS_m.o $(DIROBJ)/diago_m.o $(DIROBJ)/UtilLib_m.o $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/diago_m.o:      $(DIROBJ)/NumParameters_m.o
$(DIROBJ)/NumParameters_m.o: obj
#
############################################################################
