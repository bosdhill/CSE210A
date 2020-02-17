load ../../harness

@test "c1cf676d6bd7" {
  check 'if (x   -    0     <-2*     x   ∨x    +  x     <     -3   * -2) then skip     else   
  y:=    y     *     x  ' '⇒ skip, {}'
}
