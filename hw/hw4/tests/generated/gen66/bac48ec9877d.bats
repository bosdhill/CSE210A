load ../../harness

@test "bac48ec9877d" {
  check 'if (¬false)    then 

 y  :=     y+ -1  else 



skip' '⇒ y := (y+-1), {}
⇒ skip, {y → -1}'
}
