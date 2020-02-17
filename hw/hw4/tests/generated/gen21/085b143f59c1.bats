load ../../harness

@test "085b143f59c1" {
  check 'skip    ;
    z     :=-3   -    z   ' '⇒ z := (-3-z), {}
⇒ skip, {z → -3}'
}
