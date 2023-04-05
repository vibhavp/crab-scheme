#include "Scheme/SchemeDialect.hpp"
#include "Scheme/SchemeOps.hpp"
#include "Scheme/SchemeTypes.hpp"
#include "Scheme/SchemeAttrs.hpp"

#include <mlir/IR/Dialect.h>
#include <mlir/IR/DialectImplementation.h>
#include <mlir/IR/Attributes.h>

#include <mlir/Dialect/Async/IR/Async.h>
#include <mlir/Dialect/LLVMIR/LLVMDialect.h>
#include <mlir/IR/BuiltinDialect.h>

#include <llvm/ADT/TypeSwitch.h>

using namespace mlir;
using namespace scheme;
using namespace llvm;

#define GET_ATTRDEF_CLASSES
#include "Scheme/SchemeOpsAttrs.cpp.inc"

#include "Scheme/SchemeOpsDialect.cpp.inc"

void SchemeDialect::initialize() {
 
  addOperations<
#define GET_OP_LIST
#include "Scheme/SchemeOps.cpp.inc"
      >();
  addAttributes <
#define GET_ATTRDEF_LIST
#include "Scheme/SchemeOpsAttrs.cpp.inc"
    >();
  registerTypes();
}
