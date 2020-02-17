load ../../harness

@test "baab378f2de7" {
  check 'if (¬true) then  skip     else 
z    :=    3    *  -1   ' '⇒ z := (3*-1), {}
⇒ skip, {z → -3}'
}
