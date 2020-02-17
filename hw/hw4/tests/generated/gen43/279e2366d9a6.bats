load ../../harness

@test "279e2366d9a6" {
  check 'if (¬(bl+3<  x   *  x)) then 
 y   :=     z  -    3      else 
 
 z     :=   -3    + x    ' '⇒ y := (z-3), {}
⇒ skip, {y → -3}'
}
