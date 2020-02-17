load ../../harness

@test "a4b92927170d" {
  check 'if (-1*     z<  -1*     -1     ∨     true)     then  
skip  else skip' '⇒ skip, {}'
}
