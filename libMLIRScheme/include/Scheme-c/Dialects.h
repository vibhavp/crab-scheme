#ifndef SCHEME_C_DIALECTS_H
#define SCHEME_C_DIALECTS_H

#include "mlir-c/IR.h"

#ifdef __cplusplus
extern "C" {
#endif

MLIR_DECLARE_CAPI_DIALECT_REGISTRATION(Scheme, scheme);

#ifdef __cplusplus
}
#endif

#endif
