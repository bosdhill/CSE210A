load ../../harness

@test "2b1db9d618c3" {
  check 'if (y -   x<    z     *    x    ∧ true)   then 
 
  skip    else   y   :=-1*z' '⇒ y := (-1*z), {}
⇒ skip, {y → 0}'
}
