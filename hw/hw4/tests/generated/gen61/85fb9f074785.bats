load ../../harness

@test "85fb9f074785" {
  check 'y     :=    4   +   0 ;   
z    := y' '⇒ skip; z := y, {y → 4}
⇒ z := y, {y → 4}
⇒ skip, {y → 4, z → 4}'
}
