#=================================================================================
# gfortran (osx and linux)
#=================================================================================
ifeq ($(FFC),$(filter $(FFC),gfortran gfortran-11 gfortran-14 gfortran-15))

  # optimization management (default without optimization)
  ifeq ($(OOPT),1)
    FFLAGS = -O5 -g -fbacktrace -funroll-loops -ftree-vectorize -falign-loops=16
    CFLAGS = -O5 -g             -funroll-loops -ftree-vectorize -falign-loops=16
  else
    FFLAGS = -Og -g -fbacktrace -fcheck=all -fwhole-file -fcheck=pointer -Wuninitialized -finit-real=nan -finit-integer=nan
    CFLAGS = -O0 -g                         -fwhole-file -Wuninitialized
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -fdefault-integer-8
  endif

  # where to store .mod files
  FFLAGS +=-J$(MOD_DIR)

  # where to look .mod files (add -I$(MOD_DIR) for nagfor)
  FFLAGS += -I$(MOD_DIR) $(EXTMod)

  # omp management (default with openmp)
  ifeq ($(OOMP),1)
    FFLAGS += -fopenmp
    CFLAGS += -fopenmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # lapack management
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB = -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB = -llapack -lblas
    endif
  endif

   FC_VER = $(shell $(FFC) --version | head -1 )

endif
#=================================================================================
# lfortran (it does not work)
#=================================================================================
ifeq ($(FFC),lfortran)

  # optimization management (default without optimization)
  ifeq ($(OOPT),1)
    FFLAGS = --fast --realloc-lhs
  else
    FFLAGS = --realloc-lhs
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -fdefault-integer-8
  endif

  # where to store .mod files
  FFLAGS +=-J$(MOD_DIR)

  # where to look .mod files (add -I$(MOD_DIR) for nagfor)
  FFLAGS += -I$(MOD_DIR) $(EXTMod)

  # omp management (default with openmp)
  ifeq ($(OOMP),1)
    FFLAGS += --openmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  # lapack management
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB = -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB = -llapack -lblas
    endif
  endif

   FC_VER = $(shell $(FFC) --version | head -1 )

endif
#=================================================================================
# ifort and ifx compillation v17 v18 with/without mkl
#=================================================================================
ifeq ($(FFC),$(filter $(FFC),ifort ifx))

  # opt management + add/remove some flag to ifort/ifx (mainly to avoid bugs with ifx)
  ifeq ($(OOPT),1)
    FFLAGS = -O  -g -traceback 
    CFLAGS = -O  -g -traceback
    ifeq ($(FFC),ifort)
      FFLAGS += -heap-arrays
    endif
  else
    FFLAGS = -O0  -g -traceback
    CFLAGS = -O0  -g -traceback
    ifeq ($(FFC),ifort)
      FFLAGS += -heap-arrays -check all
    else
      FFLAGS += -check all,nouninit
    endif
  endif
  
  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -i8
  endif

  # where to store the modules
  FFLAGS +=-module $(MOD_DIR)

  # where to look .mod files (add -I$(MOD_DIR) for nagfor)
  FFLAGS += -I$(MOD_DIR) $(EXTMod)

  # omp management
  ifeq ($(OOMP),1)
    ifeq ($(FFC),ifort)
      FFLAGS += -qopenmp -parallel
      CFLAGS += -qopenmp -parallel
    else # ifx
      FFLAGS += -qopenmp
      CFLAGS += -qopenmp
    endif
  endif

  # lapack management with cpreprocessing
  FFLAGS += -cpp -D__LAPACK="$(LLAPACK)"

  ifeq ($(LLAPACK),1)
    ifeq ($(FFC),ifort)
      FLIB += -mkl -lpthread
    else # ifx
    FLIB += -qmkl -lpthread
    endif
  else
    FLIB += -lpthread
  endif

  FC_VER = $(shell $(FFC) --version | head -1 )

endif
#===============================================================================
# nag compillation (nagfor)
#===============================================================================
ifeq ($(FFC),nagfor)

  # opt management
  ifeq ($(OOPT),1)
      FFLAGS = -O4 -o -compatible -kind=byte -Ounroll=4 -s
  else
    ifeq ($(OOMP),0)
      ifeq ($(LLAPACK),0)
          FFLAGS = -O0 -g -gline -kind=byte -C -C=alias -C=intovf -C=undefined
      else
          FFLAGS = -O0 -g -gline -kind=byte -C -C=alias -C=intovf
      endif
    else
          FFLAGS = -O0 -g        -kind=byte -C -C=alias -C=intovf
    endif
  endif

  # integer kind management
  ifeq ($(INT),8)
    FFLAGS += -i8
  endif

  # where to store the .mod files
  FFLAGS +=-mdir $(MOD_DIR)

  # where to look .mod files (add -I$(MOD_DIR) for nagfor)
  FFLAGS += -I$(MOD_DIR) $(EXTMod)

  # omp management
  ifeq ($(OOMP),1)
    FFLAGS += -openmp
  endif

  # lapack management with cpreprocessing
  FFLAGS += -fpp -D__LAPACK="$(LLAPACK)"

  # lapact management (default with openmp), with cpreprocessing
  ifeq ($(LLAPACK),1)
    ifeq ($(OS),Darwin)    # OSX
      # OSX libs (included lapack+blas)
      FLIB = -framework Accelerate
    else                   # Linux
      # linux libs
      FLIB = -llapack -lblas
    endif
  endif

  FC_VER = $(shell $(FFC) -V 3>&1 1>&2 2>&3 | head -1 )

endif