load ../../harness

@test "26b478382690" {
  check 'if (¬false)  then      y     :=     J  -4 else 
 z     :=    z-1    ' '⇒ y := (J-4), {}
⇒ skip, {y → -4}'
}
