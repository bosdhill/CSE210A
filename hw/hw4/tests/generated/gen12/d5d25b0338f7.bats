load ../../harness

@test "d5d25b0338f7" {
  check 'while false    ∨     x*   0    <     x     -  z do  x    :=  -3  -   y     ' '⇒ skip, {}'
}
