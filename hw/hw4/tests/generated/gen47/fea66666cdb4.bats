load ../../harness

@test "fea66666cdb4" {
  check 'if (false ∧  3   -  1 =     0   *    -1)    then p :=     y    *  y   else y  := x *  -1  ' '⇒ y := (x*-1), {}
⇒ skip, {y → 0}'
}
