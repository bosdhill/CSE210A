load ../../harness

@test "29d9a5f69060" {
  check 'if (-1     * 3  <  4   -  z  ∨   4 *     y     <   2) then   z :=y   -    y    else  
   y:= G     -    0   ' '⇒ z := (y-y), {}
⇒ skip, {z → 0}'
}
