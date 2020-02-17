load ../../harness

@test "fc4df7d8855f" {
  check 'z  :=  0     +     0    ;
z:=   y     *     x   ' '⇒ skip; z := (y*x), {z → 0}
⇒ z := (y*x), {z → 0}
⇒ skip, {z → 0}'
}
