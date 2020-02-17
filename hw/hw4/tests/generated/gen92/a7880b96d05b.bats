load ../../harness

@test "a7880b96d05b" {
  check 'x  :=  x   +   y   ;

skip' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
