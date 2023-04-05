#ifndef SCHEMEDIALECT_TD
#define SCHEMEDIALECT_TD

include "mlir/IR/DialectBase.td"
include "mlir/IR/OpBase.td"

def SchemeDialect : Dialect {
    let summary = "LLVM MLIR Dialect for Scheme";
    let name = "scheme";
    let cppNamespace = "::scheme";
    let dependentDialects = [
    	"LLVM::LLVMDialect",
	"async::AsyncDialect",
	"::BuiltinDialect"
    ];

    let useDefaultAttributePrinterParser = 1;
    let useDefaultTypePrinterParser = 1;
    let extraClassDeclaration = [{
        void registerTypes();
    }];
    let emitAccessorPrefix = kEmitAccessorPrefix_Prefixed;
}

#endif //SCHEMEDIALECT_TD
