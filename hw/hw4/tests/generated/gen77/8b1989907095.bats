load ../../harness

@test "8b1989907095" {
  check 'if (x  *    e  < -3)  then  z :=     x  *     y  else    skip   ' '⇒ skip, {}'
}
