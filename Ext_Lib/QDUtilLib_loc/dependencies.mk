#===============================================
qdutil_file_m = $(OBJ_DIR)/File_m.o
qdutil_frac_m = $(OBJ_DIR)/Frac_m.o
qdutil_diago_m = $(OBJ_DIR)/Diago_m.o
qdutil_diagork16_m = $(OBJ_DIR)/DiagoRk16_m.o
qdutil_fft_ooura_m = $(OBJ_DIR)/FFT_m.o
qdutil_intvec_m = $(OBJ_DIR)/IntVec_m.o
qdutil_mathutil_m = $(OBJ_DIR)/MathUtil_m.o
qdutil_matrix_m = $(OBJ_DIR)/Matrix_m.o
qdutil_realvec_m = $(OBJ_DIR)/RealVec_m.o
qdutil_rw_matvec_base_m = $(OBJ_DIR)/RW_MatVec_base_m.o
qdutil_rw_matvec_m = $(OBJ_DIR)/RW_MatVec_m.o
qdutil_rw_matvec_rk16_m = $(OBJ_DIR)/RW_MatVec_Rk16_m.o
qdutil_rw_matvec_rk4_m = $(OBJ_DIR)/RW_MatVec_Rk4_m.o
qdutil_rw_matvec_rk8_m = $(OBJ_DIR)/RW_MatVec_Rk8_m.o
qdutil_vector_m = $(OBJ_DIR)/Vector_m.o
qdutil_vector_rk16_m = $(OBJ_DIR)/Vector_Rk16_m.o
qdutil_vector_rk4_m = $(OBJ_DIR)/Vector_Rk4_m.o
qdutil_vector_rk8_m = $(OBJ_DIR)/Vector_Rk8_m.o
qdutil_memory_base_m = $(OBJ_DIR)/Memory_base_m.o
qdutil_memory_m = $(OBJ_DIR)/Memory_m.o
qdutil_memory_notpointer_m = $(OBJ_DIR)/Memory_NotPointer_m.o
qdutil_memory_pointer_m = $(OBJ_DIR)/Memory_Pointer_m.o
qdutil_numparameters_m = $(OBJ_DIR)/NumParameters_m.o
qdutil_m = $(OBJ_DIR)/QDUtil_m.o
qdutil_boxab_m = $(OBJ_DIR)/BoxAB_m.o
qdutil_boxab_rk16_m = $(OBJ_DIR)/BoxAB_Rk16_m.o
qdutil_fourier_m = $(OBJ_DIR)/Fourier_m.o
qdutil_fourier_rk16_m = $(OBJ_DIR)/Fourier_Rk16_m.o
qdutil_hermiteh_m = $(OBJ_DIR)/HermiteH_m.o
qdutil_hermiteh_rk16_m = $(OBJ_DIR)/HermiteH_Rk16_m.o
qdutil_legendrep_m = $(OBJ_DIR)/LegendreP_m.o
qdutil_legendrep_rk16_m = $(OBJ_DIR)/LegendreP_Rk16_m.o
qdutil_quadrature_m = $(OBJ_DIR)/Quadrature_m.o
qdutil_quadrature_rk16_m = $(OBJ_DIR)/Quadrature_Rk16_m.o
qdutil_string_m = $(OBJ_DIR)/String_m.o
qdutil_string_rk16_m = $(OBJ_DIR)/String_Rk16_m.o
qdutil_string_rk4_m = $(OBJ_DIR)/String_Rk4_m.o
qdutil_string_rk8_m = $(OBJ_DIR)/String_Rk8_m.o
qdutil_test_m = $(OBJ_DIR)/Test_m.o
qdutil_time_m = $(OBJ_DIR)/Time_m.o
#===============================================
$(OBJ_DIR)/File_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Frac_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Diago_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_test_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/DiagoRk16_m.o : \
          $(qdutil_test_m) \
          $(qdutil_string_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/FFT_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/IntVec_m.o : \
          $(qdutil_memory_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/MathUtil_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Matrix_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/RealVec_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/RW_MatVec_base_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/RW_MatVec_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_rw_matvec_rk4_m) \
          $(qdutil_rw_matvec_rk8_m) \
          $(qdutil_rw_matvec_rk16_m) \
          $(qdutil_rw_matvec_base_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/RW_MatVec_Rk16_m.o : \
          $(qdutil_rw_matvec_base_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/RW_MatVec_Rk4_m.o : \
          $(qdutil_rw_matvec_base_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/RW_MatVec_Rk8_m.o : \
          $(qdutil_rw_matvec_base_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m)
$(OBJ_DIR)/Vector_m.o : \
          $(qdutil_vector_rk4_m) \
          $(qdutil_vector_rk8_m) \
          $(qdutil_vector_rk16_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_test_m) \
          $(qdutil_rw_matvec_m)
$(OBJ_DIR)/Vector_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Vector_Rk4_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Vector_Rk8_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Memory_base_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Memory_m.o : \
          $(qdutil_memory_base_m) \
          $(qdutil_memory_pointer_m) \
          $(qdutil_memory_notpointer_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Memory_NotPointer_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m)
$(OBJ_DIR)/Memory_Pointer_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m)
$(OBJ_DIR)/NumParameters_m.o : \
          $(iso_fortran_env) \
          $(qdutil_test_m)
$(OBJ_DIR)/QDUtil_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_mathutil_m) \
          $(qdutil_string_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_matrix_m) \
          $(qdutil_vector_m) \
          $(qdutil_diago_m) \
          $(qdutil_intvec_m) \
          $(qdutil_realvec_m) \
          $(qdutil_frac_m) \
          $(qdutil_file_m) \
          $(qdutil_time_m) \
          $(qdutil_memory_m) \
          $(qdutil_fft_ooura_m) \
          $(qdutil_quadrature_m) \
          $(qdutil_diagork16_m) \
          $(qdutil_quadrature_rk16_m)
$(OBJ_DIR)/BoxAB_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/BoxAB_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Fourier_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Fourier_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/HermiteH_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/HermiteH_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/LegendreP_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/LegendreP_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Quadrature_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_diago_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_hermiteh_m) \
          $(qdutil_boxab_m) \
          $(qdutil_fourier_m) \
          $(qdutil_legendrep_m) \
          $(qdutil_quadrature_rk16_m) \
          $(qdutil_matrix_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/Quadrature_Rk16_m.o : \
          $(qdutil_numparameters_m) \
          $(qdutil_string_m) \
          $(qdutil_diagork16_m) \
          $(qdutil_rw_matvec_m) \
          $(qdutil_hermiteh_rk16_m) \
          $(qdutil_boxab_rk16_m) \
          $(qdutil_fourier_rk16_m) \
          $(qdutil_legendrep_rk16_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/String_m.o : \
          $(qdutil_string_rk4_m) \
          $(qdutil_string_rk8_m) \
          $(qdutil_string_rk16_m) \
          $(qdutil_numparameters_m) \
          $(qdutil_memory_base_m) \
          $(qdutil_test_m)
$(OBJ_DIR)/String_Rk16_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/String_Rk4_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/String_Rk8_m.o : \
          $(qdutil_numparameters_m)
$(OBJ_DIR)/Time_m.o : \
          $(qdutil_numparameters_m)
