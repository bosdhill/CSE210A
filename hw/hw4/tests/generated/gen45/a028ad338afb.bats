load ../../harness

@test "a028ad338afb" {
  check 'y  :=  x*z ;   
x:=-3     +-4' '⇒ skip; x := (-3+-4), {y → 0}
⇒ x := (-3+-4), {y → 0}
⇒ skip, {x → -7, y → 0}'
}
