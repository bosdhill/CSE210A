load ../../harness

@test "8a0b0a12610c" {
  check 'if (-1  + 4<    u2  + 0    ∨ 2  * 0    = -2   +    y)      then   z:=    2   *-1  else  z :=    y   *x' '⇒ z := (y*x), {}
⇒ skip, {z → 0}'
}
