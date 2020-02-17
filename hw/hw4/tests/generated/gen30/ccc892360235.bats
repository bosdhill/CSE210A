load ../../harness

@test "ccc892360235" {
  check 'if (false     ∨   true)   then   
y :=   z    *z else y   :=   y-   -3  ' '⇒ y := (z*z), {}
⇒ skip, {y → 0}'
}
