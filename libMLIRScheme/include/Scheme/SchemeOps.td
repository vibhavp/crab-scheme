#ifndef SCHEMEOPS_TD
#define SCHEMEOPS_TD

include "SchemeDialect.td"
include "SchemeTypes.td"
include "SchemeAttrs.td"

include "mlir/IR/OpBase.td"
include "mlir/Interfaces/InferTypeOpInterface.td"

class Scheme_Op<string mnemonic, list<Trait> traits = []> :
      Op<SchemeDialect, mnemonic, traits>;

def Scheme_EqOp : Scheme_Op<"eq", []> {
    let arguments = (ins Scheme_TaggedObjectAttr:$obj1, Scheme_TaggedObjectAttr:$obj2);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_NumberAddOp : Scheme_Op<"number.add", []> {
    let arguments = (ins Scheme_TaggedObjectType:$lhs, Scheme_TaggedObjectType:$rhs);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_NumberSubOp : Scheme_Op<"number.sub", []> {
    let arguments = (ins Scheme_TaggedObjectType:$lhs, Scheme_TaggedObjectType:$rhs);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_NumberMulOp : Scheme_Op<"number.mul", []> {
    let arguments = (ins Scheme_TaggedObjectType:$lhs, Scheme_TaggedObjectType:$rhs);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_NumberDivOp : Scheme_Op<"number.div", []> {
    let arguments = (ins Scheme_TaggedObjectType:$lhs, Scheme_TaggedObjectType:$rhs);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_NumberRemOp : Scheme_Op<"number.rem", []> {
    let arguments = (ins Scheme_TaggedObjectType:$lhs, Scheme_TaggedObjectType:$rhs);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_ConsOp : Scheme_Op<"cons", []> {
    let arguments = (ins Scheme_TaggedObjectType:$car, Scheme_TaggedObjectType:$cdr);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_PairCarOp : Scheme_Op<"pair.car", []> {
    let arguments = (ins Scheme_TaggedObjectType:$pair);
    let results = (outs Scheme_TaggedObjectType:$result);
}

def Scheme_PairCdrOp : Scheme_Op<"pair.cdr", []> {
    let arguments = (ins Scheme_TaggedObjectType:$pair);
    let results = (outs Scheme_TaggedObjectType:$result);
}

#endif
