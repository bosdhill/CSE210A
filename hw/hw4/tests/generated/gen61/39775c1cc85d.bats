load ../../harness

@test "39775c1cc85d" {
  check 'while i1    +     4<  1   +    -1  ∨   W0 *z <y   do x:=   -2  +     x' '⇒ skip, {}'
}
