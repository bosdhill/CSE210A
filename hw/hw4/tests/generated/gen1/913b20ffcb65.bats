load ../../harness

@test "913b20ffcb65" {
  check 'while false  ∨     -4 +x  = x     *  z do w :=f  ' '⇒ skip, {}'
}
