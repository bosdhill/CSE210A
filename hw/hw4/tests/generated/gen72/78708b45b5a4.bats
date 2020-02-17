load ../../harness

@test "78708b45b5a4" {
  check 'skip  ;j3  :=    3 -    -3 ' '⇒ j3 := (3--3), {}
⇒ skip, {j3 → 6}'
}
