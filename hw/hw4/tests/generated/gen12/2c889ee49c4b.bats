load ../../harness

@test "2c889ee49c4b" {
  check 'while x    -  s <     x    +-1∨  0    - 3 = 1   +   -3     do x    :=y    +  x  ' '⇒ skip, {}'
}
