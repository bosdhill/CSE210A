load ../../harness

@test "34dbf73a057d" {
  check 'if (¬false)  then x    := 4  else z :=    R4  *0  ' '⇒ x := 4, {}
⇒ skip, {x → 4}'
}
