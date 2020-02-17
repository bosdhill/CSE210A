load ../../harness

@test "342aada538be" {
  check 'if (¬(-3*    4    < 2     -  -4))   then  skip else  skip ' '⇒ skip, {}'
}
