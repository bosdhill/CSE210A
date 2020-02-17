load ../../harness

@test "717cb8835f20" {
  check 'if (¬(F -   y    =     0   *    y)) then 
 skip  else 
y   := -3 + -2     ' '⇒ y := (-3+-2), {}
⇒ skip, {y → -5}'
}
