load ../../harness

@test "f2c8ead36197" {
  check 'if true   then 
z :=    -4    - 1      else skip    ' '⇒ z := (-4-1), {}
⇒ skip, {z → -5}'
}
