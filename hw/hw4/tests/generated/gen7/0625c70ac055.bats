load ../../harness

@test "0625c70ac055" {
  check 'if (¬false)   then z  :=y     +z      else 
 y     :=    X7  *1  ' '⇒ z := (y+z), {}
⇒ skip, {z → 0}'
}
