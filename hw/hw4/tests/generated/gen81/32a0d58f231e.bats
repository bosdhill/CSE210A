load ../../harness

@test "32a0d58f231e" {
  check 'x     :=    -2  -3 ; 
z:=   -1     ' '⇒ skip; z := -1, {x → -5}
⇒ z := -1, {x → -5}
⇒ skip, {x → -5, z → -1}'
}
