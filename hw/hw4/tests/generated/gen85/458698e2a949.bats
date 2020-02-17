load ../../harness

@test "458698e2a949" {
  check 'if (z  = 2  +2 * 3  ∧     false)     then 


  y    :=     3     else y:=z   *     -4   ' '⇒ y := (z*-4), {}
⇒ skip, {y → 0}'
}
