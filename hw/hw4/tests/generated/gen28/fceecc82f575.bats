load ../../harness

@test "fceecc82f575" {
  check 'if (false  ∧    x   -    x=  4  - -2)      then r0  :=-4- y else r3     :=   v     +   A ' '⇒ r3 := (v+A), {}
⇒ skip, {r3 → 0}'
}
