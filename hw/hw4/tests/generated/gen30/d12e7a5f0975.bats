load ../../harness

@test "d12e7a5f0975" {
  check 'if (¬false)    then  

y    :=4     else skip' '⇒ y := 4, {}
⇒ skip, {y → 4}'
}
