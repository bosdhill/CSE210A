load ../../harness

@test "7286a7a4ed5e" {
  check 'if (false ∧    bN    =    z  -   x)      then   x    :=  x-    -2     else      skip' '⇒ skip, {}'
}
