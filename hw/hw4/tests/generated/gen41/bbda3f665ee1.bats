load ../../harness

@test "bbda3f665ee1" {
  check 'if (false ∨ z+  0    =   -3+   4)   then    AG   := z   -   1      else x   := 0  *x   ' '⇒ x := (0*x), {}
⇒ skip, {x → 0}'
}
