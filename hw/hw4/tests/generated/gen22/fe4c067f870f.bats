load ../../harness

@test "fe4c067f870f" {
  check 'if (false    ∧ false)      then   z:=  y*   Q     else 
x    :=  -1     + 2     ' '⇒ x := (-1+2), {}
⇒ skip, {x → 1}'
}
