$(OBJ_DIR)/File_m.o: $(OBJ_DIR)/String_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/Frac_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/Diago_m.o: $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/DiagoRk16_m.o: $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/FFT_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/IntVec_m.o: $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/Memory_m.o $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/MathUtil_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/Matrix_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/RealVec_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/RW_MatVec_base_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/RW_MatVec_m.o: $(OBJ_DIR)/RW_MatVec_Rk16_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/RW_MatVec_Rk8_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/RW_MatVec_Rk4_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/RW_MatVec_base_m.o
$(OBJ_DIR)/RW_MatVec_Rk16_m.o: $(OBJ_DIR)/RW_MatVec_base_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/RW_MatVec_Rk4_m.o: $(OBJ_DIR)/RW_MatVec_base_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/RW_MatVec_Rk8_m.o: $(OBJ_DIR)/String_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/RW_MatVec_base_m.o
$(OBJ_DIR)/Vector_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Vector_Rk4_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/Vector_Rk16_m.o $(OBJ_DIR)/Vector_Rk8_m.o $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/Vector_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Vector_Rk4_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Vector_Rk8_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Memory_base_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Memory_m.o: $(OBJ_DIR)/Memory_base_m.o $(OBJ_DIR)/Memory_Pointer_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/Memory_NotPointer_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/String_m.o
$(OBJ_DIR)/Memory_NotPointer_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_base_m.o
$(OBJ_DIR)/Memory_Pointer_m.o: $(OBJ_DIR)/Memory_base_m.o $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/NumParameters_m.o: $(OBJ_DIR)/Test_m.o
$(OBJ_DIR)/BoxAB_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/BoxAB_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Fourier_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Fourier_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/HermiteH_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/HermiteH_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/LegendreP_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/LegendreP_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Quadrature_m.o: $(OBJ_DIR)/LegendreP_m.o $(OBJ_DIR)/Matrix_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/HermiteH_m.o $(OBJ_DIR)/Fourier_m.o $(OBJ_DIR)/Quadrature_Rk16_m.o $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/Diago_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/BoxAB_m.o
$(OBJ_DIR)/Quadrature_Rk16_m.o: $(OBJ_DIR)/RW_MatVec_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/LegendreP_Rk16_m.o $(OBJ_DIR)/BoxAB_Rk16_m.o $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/HermiteH_Rk16_m.o $(OBJ_DIR)/Fourier_Rk16_m.o $(OBJ_DIR)/DiagoRk16_m.o
$(OBJ_DIR)/String_m.o: $(OBJ_DIR)/NumParameters_m.o $(OBJ_DIR)/Memory_base_m.o $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/String_Rk16_m.o $(OBJ_DIR)/String_Rk4_m.o $(OBJ_DIR)/String_Rk8_m.o
$(OBJ_DIR)/String_Rk16_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/String_Rk4_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/String_Rk8_m.o: $(OBJ_DIR)/NumParameters_m.o
$(OBJ_DIR)/Time_m.o: $(OBJ_DIR)/Test_m.o $(OBJ_DIR)/String_m.o $(OBJ_DIR)/NumParameters_m.o
