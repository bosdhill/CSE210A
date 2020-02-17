load ../../harness

@test "ebb06a96ca58" {
  check 'while z     +     x  <   x-1   ∧  z *  0<   4   -    3    do     
x    :=-1    +    -2   ' '⇒ skip, {}'
}
