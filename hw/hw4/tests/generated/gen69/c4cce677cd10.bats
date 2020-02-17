load ../../harness

@test "c4cce677cd10" {
  check 'if (4     <   2 --4 ∨   2 *   -3    =x   *     x)     then  
s :=y -  Mc     else  z   := y -  4     ' '⇒ s := (y-Mc), {}
⇒ skip, {s → 0}'
}
