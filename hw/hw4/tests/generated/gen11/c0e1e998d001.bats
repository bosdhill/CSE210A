load ../../harness

@test "c0e1e998d001" {
  check 'if (3   -   1 = 2   *x ∧-3    *2   <3 +   4)   then     
skip     else M :=  y   *(-4     -   x)   ' '⇒ M := (y*(-4-x)), {}
⇒ skip, {M → 0}'
}
