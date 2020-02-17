load ../../harness

@test "702c449f5d6f" {
  check 'if (y    - L  =    y  *     -3∨y  *  -1    <   s+   1) then a6 :=   y      else 
x :=x  -z   ' '⇒ a6 := y, {}
⇒ skip, {a6 → 0}'
}
