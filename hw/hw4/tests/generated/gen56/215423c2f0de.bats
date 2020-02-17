load ../../harness

@test "215423c2f0de" {
  check 'skip   ;   x:=  -3 *4     ' '⇒ x := (-3*4), {}
⇒ skip, {x → -12}'
}
