load ../../harness

@test "f25877c8870c" {
  check 'if (¬false) then  z  :=   1     *    C      else 
 skip  ' '⇒ z := (1*C), {}
⇒ skip, {z → 0}'
}
