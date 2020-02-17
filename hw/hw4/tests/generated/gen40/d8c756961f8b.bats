load ../../harness

@test "d8c756961f8b" {
  check 'if (¬false)      then 
  y   :=     -3 else 
  skip' '⇒ y := -3, {}
⇒ skip, {y → -3}'
}
