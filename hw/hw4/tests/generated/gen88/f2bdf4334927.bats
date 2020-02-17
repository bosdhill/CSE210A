load ../../harness

@test "f2bdf4334927" {
  check 'z  :=    a*     x; skip   ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
