load ../../harness

@test "31fb3d0d697b" {
  check 'y   :=  C1 *    z    ;  z  :=  -1  *     -2' '⇒ skip; z := (-1*-2), {y → 0}
⇒ z := (-1*-2), {y → 0}
⇒ skip, {y → 0, z → 2}'
}
