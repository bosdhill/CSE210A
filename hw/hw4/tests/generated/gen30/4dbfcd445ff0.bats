load ../../harness

@test "4dbfcd445ff0" {
  check 'skip   ;  q  :=  z  - y    ' '⇒ q := (z-y), {}
⇒ skip, {q → 0}'
}
