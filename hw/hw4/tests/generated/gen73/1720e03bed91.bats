load ../../harness

@test "1720e03bed91" {
  check 'x    := -1  *y  ;

 

skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
