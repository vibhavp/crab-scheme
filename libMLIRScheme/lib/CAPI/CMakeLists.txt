add_mlir_public_c_api_library(SchemeCAPI
  Dialects.cpp
  LINK_LIBS PUBLIC
  MLIRScheme
)
