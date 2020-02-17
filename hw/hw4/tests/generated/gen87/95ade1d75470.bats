load ../../harness

@test "95ade1d75470" {
  check 'y  :=  x  +  z  ' '⇒ skip, {y → 0}'
}
