load ../../harness

@test "98c64a03c214" {
  check 'y     :=    -4*   x ; 
x     :=     z ' '⇒ skip; x := z, {y → 0}
⇒ x := z, {y → 0}
⇒ skip, {x → 0, y → 0}'
}
