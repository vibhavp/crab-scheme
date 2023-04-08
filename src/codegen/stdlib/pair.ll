%Pair = type {
      ; %GCData,
      ptr addrspace(1), ; car
      ptr ; cdr
}

declare ptr addrspace(1) @llvm.ptrmask.p0.i64(ptr addrspace(1), i64)


define private ptr addrspace(1) @car(ptr addrspace(1) %object) {
       %tag = shl i64 u0x01, 56
       %pair = call ptr addrspace(1) @llvm.ptrmask.p0.i64(ptr addrspace(1) %object, i64 %tag)
       %car = getelementptr inbounds %Pair, ptr addrspace(1) %pair, i32 0
       ret ptr addrspace(1) %car
}

; define private ptr addrspace(1) @cdr(ptr addrspace(1))
