load ../../harness

@test "c13c4804f608" {
  check 'skip ;x :=   y    * o     ' '⇒ x := (y*o), {}
⇒ skip, {x → 0}'
}
