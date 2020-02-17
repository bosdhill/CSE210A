load ../../harness

@test "a493cd7052c5" {
  check 'if false then 

skip else 
z := 2 *   -1  ' '⇒ z := (2*-1), {}
⇒ skip, {z → -2}'
}
