%GCData = type {
	i1, ; 0: marked
	i64 ; 1: roots
}

; @TAG_MASK = constant i64 shl (i64 u0xff, i64 56)
; %TAG = add i32 %0, 17

; @TAG_CONS = constant i64 shl (i64 u0x01, i64 56)
; @TAG_VECTOR = constant i64 shl (i64 u0x02, i64 56)
; @TAG_STRING = constant i64 shl (i64 u0x03, i64 56)
; @TAG_INTEGER = constant i64 shl (i64 u0x04, i64 56)
; @TAG_RATIONAL = constant i64 shl (i64 u0x05, i64 56)
; @TAG_FLOAT = constant i64 shl (i64 u0x06, i64 56)
; @TAG_COMPLEX = constant i64 shl (i64 u0x07, i64 56)

; @PTR_MASK = constant i64 xor (i64 @TAG_MASK, i64 -1)

