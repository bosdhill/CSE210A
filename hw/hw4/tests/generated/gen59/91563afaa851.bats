load ../../harness

@test "91563afaa851" {
  check 'if (-4     +z =   0  +   0   ∨    false)  then z:=   z  *   1   else  z    :=  4* 1    ' '⇒ z := (4*1), {}
⇒ skip, {z → 4}'
}
