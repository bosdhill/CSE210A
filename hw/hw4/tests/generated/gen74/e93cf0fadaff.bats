load ../../harness

@test "e93cf0fadaff" {
  check 'skip ;
x:=     x - -4' '⇒ x := (x--4), {}
⇒ skip, {x → 4}'
}
