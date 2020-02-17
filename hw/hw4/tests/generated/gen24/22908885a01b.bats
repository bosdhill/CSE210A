load ../../harness

@test "22908885a01b" {
  check 'skip ;  x  :=    x    * 4     ' '⇒ x := (x*4), {}
⇒ skip, {x → 0}'
}
