load ../../harness

@test "30da96c3f025" {
  check 'if (¬(z*z =     -4 *    4))  then  sn    :=    3  *   x  else 

skip' '⇒ sn := (3*x), {}
⇒ skip, {sn → 0}'
}
