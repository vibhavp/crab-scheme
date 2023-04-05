set(LLVM_TARGET_DEFINITIONS SchemeAttrs.td)
mlir_tablegen(SchemeOpsAttrs.hpp.inc -gen-attrdef-decls --attrdefs-dialect="scheme")
mlir_tablegen(SchemeOpsAttrs.cpp.inc -gen-attrdef-defs --attrdefs-dialect="scheme")
add_public_tablegen_target(MLIRSchemeAttrsIncGen)

add_mlir_dialect(SchemeOps scheme)
add_mlir_doc(SchemeOps SchemeOps Scheme/ -gen-op-doc)
add_mlir_doc(SchemeTypes SchemeTypes Scheme/ -gen-typedef-doc)
add_mlir_doc(SchemeAttrs SchemeAttrs Scheme/ -gen-attrdef-doc)
