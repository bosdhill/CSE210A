load ../../harness

@test "52851320950b" {
  check 'if (¬false)  then x  :=     x  +    3     else skip     ' '⇒ x := (x+3), {}
⇒ skip, {x → 3}'
}
