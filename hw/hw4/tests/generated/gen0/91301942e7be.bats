load ../../harness

@test "91301942e7be" {
  check 'if (-2    +  z<(Hh - x)  *   2     ∧    -2 -  2 <   1   -  z) then  
x  :=-4  else  J:= -4   * z ' '⇒ x := -4, {}
⇒ skip, {x → -4}'
}
