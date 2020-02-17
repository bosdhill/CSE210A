load ../../harness

@test "93355d115f53" {
  check 'if (z     - x    =-3  - y∨   false)    then 

y  := 3  else skip   ' '⇒ skip, {}'
}
