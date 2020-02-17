load ../../harness

@test "2525fd9b6012" {
  check 'while 3+  x    =-3 +     y ∧  1    +    -4    <   -1     +2  do y :=  z  -  1     ' '⇒ skip, {}'
}
