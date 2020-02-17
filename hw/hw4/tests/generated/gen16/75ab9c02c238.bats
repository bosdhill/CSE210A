load ../../harness

@test "75ab9c02c238" {
  check 'if (-4     <   y  *   y∨   true) then skip     else 
y    :=     3 +     4 ' '⇒ skip, {}'
}
