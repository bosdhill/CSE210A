load ../../harness

@test "de0bb44ef49d" {
  check 'if (0  - -3  <  B-     z  ∧     -3+     z    =    y+-2)     then 
 skip      else   x  :=0    -   0     ' '⇒ x := (0-0), {}
⇒ skip, {x → 0}'
}
