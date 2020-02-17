load ../../harness

@test "6effec675f5e" {
  check 'y   :=     4  *   -2     ; 

 y:= -3   ' '⇒ skip; y := -3, {y → -8}
⇒ y := -3, {y → -8}
⇒ skip, {y → -3}'
}
