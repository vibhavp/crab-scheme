#include "Scheme-c/Dialects.h"

#include "Scheme/SchemeDialect.hpp"
#include "mlir/CAPI/Registration.h"

MLIR_DEFINE_CAPI_DIALECT_REGISTRATION(Scheme, scheme, scheme::SchemeDialect)
