load ../../harness

@test "a8546008ff29" {
  check 'x:= 4   +     x     ;

  
skip' '⇒ skip; skip, {x → 4}
⇒ skip, {x → 4}'
}
