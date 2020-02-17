load ../../harness

@test "fcfafee60f2b" {
  check 'if (false ∨     false)  then   rX     :=4    -  y else x   :=     z    - -1   ' '⇒ x := (z--1), {}
⇒ skip, {x → 1}'
}
