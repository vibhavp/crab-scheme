#ifndef SCHEME_SCHEMEOPS_H
#define SCHEME_SCHEMEOPS_H

#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Dialect.h"
#include "mlir/IR/OpDefinition.h"
#include "mlir/Interfaces/InferTypeOpInterface.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"

#include "Scheme/SchemeTypes.hpp"
#include "Scheme/SchemeAttrs.hpp"

#define GET_OP_CLASSES
#include "Scheme/SchemeOps.h.inc"

#endif // SCHEME_SCHEMEOPS_H
