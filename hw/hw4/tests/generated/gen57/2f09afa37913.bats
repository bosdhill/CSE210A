load ../../harness

@test "2f09afa37913" {
  check 'if (¬false)     then y :=    1     else     
skip     ' '⇒ y := 1, {}
⇒ skip, {y → 1}'
}
