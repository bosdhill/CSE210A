load ../../harness

@test "817cc2bce41b" {
  check 'y   :=  -1   - y;  
skip' '⇒ skip; skip, {y → -1}
⇒ skip, {y → -1}'
}
