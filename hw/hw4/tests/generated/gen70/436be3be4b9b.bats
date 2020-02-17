load ../../harness

@test "436be3be4b9b" {
  check 'z  :=  -4  *2   ;z:=    -2    + 1    ' '⇒ skip; z := (-2+1), {z → -8}
⇒ z := (-2+1), {z → -8}
⇒ skip, {z → -1}'
}
