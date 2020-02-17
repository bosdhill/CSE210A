load ../../harness

@test "389c1218f50b" {
  check 'if (¬(-2     *x=   z  +   -1))      then skip      else   skip ' '⇒ skip, {}'
}
