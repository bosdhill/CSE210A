load ../../harness

@test "daf9c536b712" {
  check 'x  :=   y    -     4   ;

  x :=    y  +  -1    ' '⇒ skip; x := (y+-1), {x → -4}
⇒ x := (y+-1), {x → -4}
⇒ skip, {x → -1}'
}
