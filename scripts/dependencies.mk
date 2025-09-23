#===============================================
addnsvm_dnfunc_m = $(OBJ_DIR)/dnFunc_m.o
addnsvm_dnmat_m = $(OBJ_DIR)/dnMat_m.o
addnsvm_dnpoly_m = $(OBJ_DIR)/dnPoly_m.o
addnsvm_dns_m = $(OBJ_DIR)/dnS_m.o
addnsvm_dns_op_m = $(OBJ_DIR)/dnS_Op_m.o
addnsvm_dnvec_m = $(OBJ_DIR)/dnVec_m.o
addnsvm_m = $(OBJ_DIR)/dnSVM_m.o
#===============================================
#file+mod_name: SRC/ADdnSVM/dnFunc_m.f90 addnsvm_dnfunc_m
$(OBJ_DIR)/dnFunc_m.o : \
          $(qdutil_m) \
          $(addnsvm_dns_m) \
          $(addnsvm_dnpoly_m)
#file+mod_name: SRC/ADdnSVM/dnMat_m.f90 addnsvm_dnmat_m
$(OBJ_DIR)/dnMat_m.o : \
          $(qdutil_m) \
          $(addnsvm_dns_m)
#file+mod_name: SRC/ADdnSVM/dnPoly_m.f90 addnsvm_dnpoly_m
$(OBJ_DIR)/dnPoly_m.o : \
          $(qdutil_m) \
          $(addnsvm_dns_m)
#file+mod_name: SRC/ADdnSVM/dnS_m.f90 addnsvm_dns_m
$(OBJ_DIR)/dnS_m.o : \
          $(qdutil_m)
#file+mod_name: SRC/ADdnSVM/dnS_Op_m.f90 addnsvm_dns_op_m
$(OBJ_DIR)/dnS_Op_m.o : \
          $(qdutil_m) \
          $(addnsvm_dns_m)
#file+mod_name: SRC/ADdnSVM/dnVec_m.f90 addnsvm_dnvec_m
$(OBJ_DIR)/dnVec_m.o : \
          $(qdutil_m) \
          $(addnsvm_dns_m)
#file+mod_name: SRC/dnSVM_m.f90 addnsvm_m
$(OBJ_DIR)/dnSVM_m.o : \
          $(addnsvm_dns_m) \
          $(addnsvm_dnpoly_m) \
          $(addnsvm_dnfunc_m) \
          $(addnsvm_dns_op_m) \
          $(addnsvm_dnmat_m) \
          $(addnsvm_dnvec_m) \
          $(qdutil_numparameters_m)
