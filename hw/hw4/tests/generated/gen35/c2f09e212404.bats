load ../../harness

@test "c2f09e212404" {
  check 'if (true ∨    false) then skip      else   
z  :=   z- 0' '⇒ skip, {}'
}
