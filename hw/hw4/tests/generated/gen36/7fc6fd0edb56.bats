load ../../harness

@test "7fc6fd0edb56" {
  check 'if (false ∧  M0 + y <     -2  *    y)   then   skip     else  

y :=  y*x' '⇒ y := (y*x), {}
⇒ skip, {y → 0}'
}
