load ../../harness

@test "7662e20af070" {
  check 'z    :=   i     *4;skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
