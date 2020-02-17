load ../../harness

@test "b8770521b67e" {
  check 'if (false ∧  4+   -4   =     z   -   x)      then  skip      else 
x    :=   x *    1    ' '⇒ x := (x*1), {}
⇒ skip, {x → 0}'
}
