load ../../harness

@test "0376b092d82b" {
  check 'if (false  ∨  -1 +-4 =     4     - -1) then 
skip   else   skip    ' '⇒ skip, {}'
}
