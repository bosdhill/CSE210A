load ../../harness

@test "a29123fc763a" {
  check 'y  :=    z     +  -3   ;  
  skip     ' '⇒ skip; skip, {y → -3}
⇒ skip, {y → -3}'
}
