load ../../harness

@test "9dbd6f5fe8b0" {
  check 'while 0 +     1   =    b  ∨    0 -    0  <   z     *  z   do z    :=  z   -y ' '⇒ skip, {}'
}
