load ../../harness

@test "40d875f9d45e" {
  check 'ut  :=C    -   -1;x     :=z' '⇒ skip; x := z, {ut → 1}
⇒ x := z, {ut → 1}
⇒ skip, {ut → 1, x → 0}'
}
