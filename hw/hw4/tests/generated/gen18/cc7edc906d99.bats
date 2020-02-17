load ../../harness

@test "cc7edc906d99" {
  check 'if (z   - 3< x +     z)      then   x   :=   3  else  

z    := 0    -    x    ' '⇒ x := 3, {}
⇒ skip, {x → 3}'
}
