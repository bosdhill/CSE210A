load ../../harness

@test "c3f2650261da" {
  check 'skip   ;z    :=  y +     -2   ' '⇒ z := (y+-2), {}
⇒ skip, {z → -2}'
}
