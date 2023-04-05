add_mlir_dialect_library(MLIRScheme
	SchemeTypes.cpp
	SchemeDialect.cpp
	SchemeOps.cpp
	SchemeAttrs.cpp

	ADDITIONAL_HEADER_DIRS
	${PROTJECT_SOURCE_DIR}/include/Scheme

	DEPENDS
	MLIRSchemeOpsIncGen

	LINK_LIBS PUBLIC
	MLIRIR
	MLIRInferTypeOpInterface
)
