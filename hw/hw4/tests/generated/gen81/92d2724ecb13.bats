load ../../harness

@test "92d2724ecb13" {
  check 'while false     ∨    -1    *    z     <   -4+z    do  j:=  -1     -  tE' '⇒ skip, {}'
}
