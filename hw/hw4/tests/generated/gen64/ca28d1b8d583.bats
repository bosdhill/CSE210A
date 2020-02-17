load ../../harness

@test "ca28d1b8d583" {
  check 'y  :=     z  +  -3  ; 
  z   := y*y    ' '⇒ skip; z := (y*y), {y → -3}
⇒ z := (y*y), {y → -3}
⇒ skip, {y → -3, z → 9}'
}
