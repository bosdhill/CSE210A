load ../../harness

@test "538bdc8b8d53" {
  check 'y:= -1;
 
skip' '⇒ skip; skip, {y → -1}
⇒ skip, {y → -1}'
}
