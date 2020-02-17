load ../../harness

@test "6468a9f16696" {
  check 'if (z     * y   =     -1 - 4     ∧  x    +  -4   =  2) then skip else 
 x  :=y    *     x     ' '⇒ x := (y*x), {}
⇒ skip, {x → 0}'
}
