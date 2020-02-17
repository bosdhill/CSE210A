load ../../harness

@test "a47c284f9768" {
  check 'if (¬false)   then y  := 1    *1 else  X     :=   y +     -3    ' '⇒ y := (1*1), {}
⇒ skip, {y → 1}'
}
