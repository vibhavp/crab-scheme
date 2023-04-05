#ifndef SCHEMETYPES_TD
#define SCHEMETYPES_TD

include "mlir/IR/AttrTypeBase.td"
include "mlir/IR/BuiltinTypeInterfaces.td"

include "SchemeDialect.td"

class SchemeType<string name, list<Trait> traits = []>
      : TypeDef<SchemeDialect, name, traits> {
}

def Scheme_TaggedObjectType : SchemeType<"TaggedObject"> {
    let mnemonic = "object";
    let summary = "a tagged object";
}

def Scheme_PairType : SchemeType<"Pair", [MemRefElementTypeInterface]> {
    let mnemonic = "pair";
    let summary = "a scheme pair";
}

def Scheme_IntegerType : SchemeType<"Integer", [MemRefElementTypeInterface]> {
    let mnemonic = "integer";
    let summary = "an integer";
}

#endif
