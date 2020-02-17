load ../../harness

@test "62c99351e079" {
  check 'x     :=     1+ -1  ' '⇒ skip, {x → 0}'
}
