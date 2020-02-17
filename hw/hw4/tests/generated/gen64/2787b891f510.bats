load ../../harness

@test "2787b891f510" {
  check 'if (x    *   z   =   -4* 2   ∨ -4   * dt     =   x    +     z)   then z :=   0 else 
skip     ' '⇒ z := 0, {}
⇒ skip, {z → 0}'
}
