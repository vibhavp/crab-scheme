#include "Scheme/SchemeTypes.hpp"
#include "Scheme/SchemeDialect.hpp"

#include "mlir/IR/Builders.h"
#include "mlir/IR/DialectImplementation.h"
#include "llvm/ADT/TypeSwitch.h"

using namespace scheme;

#define GET_TYPEDEF_CLASSES
#include "Scheme/SchemeOpsTypes.cpp.inc"

void SchemeDialect::registerTypes() {
  addTypes<
#define GET_TYPEDEF_LIST
#include "Scheme/SchemeOpsTypes.cpp.inc"
      >();
}
