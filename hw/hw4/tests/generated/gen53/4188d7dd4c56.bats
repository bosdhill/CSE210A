load ../../harness

@test "4188d7dd4c56" {
  check 'if (¬false) then y  :=    3 + 0 else 

x  :=   y' '⇒ y := (3+0), {}
⇒ skip, {y → 3}'
}
