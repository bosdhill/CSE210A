load ../../harness

@test "505d519b36c4" {
  check 'y    :=    z    + -1     ; 
 z    :=   -2     -  li ' '⇒ skip; z := (-2-li), {y → -1}
⇒ z := (-2-li), {y → -1}
⇒ skip, {y → -1, z → -2}'
}
