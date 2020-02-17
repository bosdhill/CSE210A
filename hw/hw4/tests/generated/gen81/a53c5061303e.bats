load ../../harness

@test "a53c5061303e" {
  check 'y  :=4  +3    ; 
   y    :=Wx    ' '⇒ skip; y := Wx, {y → 7}
⇒ y := Wx, {y → 7}
⇒ skip, {y → 0}'
}
