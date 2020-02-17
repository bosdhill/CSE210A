load ../../harness

@test "4c12e8872a21" {
  check 'if (-4   *3    =  -1    -    y∧   false) then  skip     else skip    ' '⇒ skip, {}'
}
