#ifndef SCHEMEATTRS_ID
#define SCHEMEATTRS_TD

include "mlir/IR/AttrTypeBase.td"
include "mlir/IR/OpBase.td"
include "mlir/IR/BuiltinAttributes.td"

include "SchemeDialect.td"
include "SchemeTypes.td"

class SchemeAttr<string name, string attrMnemonic, list<Trait> traits = [], string baseCppClass = "::mlir::Attribute">
    : AttrDef<SchemeDialect, name, traits, baseCppClass> {
    let mnemonic = attrMnemonic;
}

def Scheme_TaggedObjectAttr : SchemeAttr<"TaggedObject", "tagged_object"> {
    let summary = "A tagged object";
}

// def Scheme_AnySchemeType : AnyTypeOf<[Scheme_PairType, Scheme_IntegerType]>;

// def Scheme_TaggedObjectAttr : SchemeAttr<"TaggedObject", "tagged_obj"> {
//     let parameters = (ins AttributeSelfTypeParameter<"">:$self, AnyTypeOf<[Scheme_PairType, Scheme_IntegerType]>:$value);
//     let assemblyFormat = "`<` $value `>`";
// }

// def Scheme_IntegerAttr : SchemeAttr<"Integer", "integer"> {
//     let parameters = (ins AttributeSelfTypeParameter<"">:$type, Builtin_IntegerAttr:$value);
//     let assemblyFormat = "`<` $value `>`";

//     let builders = [
//     AttrBuilderWithInferredContext<(ins "mlir::Type":$type,
//                                         "const llvm::APInt &":$value), [{
//       return $_get(type.getContext(), type, value);
//     }]>
//   ];

//     let genVerifyDecl = 1;
//     let skipDefaultBuilders = 1;
// }

// def Scheme_PairAttr : SchemeAttr<"Pair", "pair"> {
//     let parameters = (ins AttributeSelfTypeParameter<"">:$type);
//     let assemblyFormat = "";
// }

#endif
