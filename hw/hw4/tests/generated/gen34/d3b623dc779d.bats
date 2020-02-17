load ../../harness

@test "d3b623dc779d" {
  check 'if (2     -   1  <-2    *    -4    ∨     false) then 
skip  else skip    ' '⇒ skip, {}'
}
