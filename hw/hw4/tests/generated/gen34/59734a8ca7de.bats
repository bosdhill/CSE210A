load ../../harness

@test "59734a8ca7de" {
  check 'if (¬(pH + H     <    A8     * -2))  then y   :=   x  +2   else  
 x    := x    + 1' '⇒ y := (x+2), {}
⇒ skip, {y → 2}'
}
