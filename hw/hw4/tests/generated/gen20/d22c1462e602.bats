load ../../harness

@test "d22c1462e602" {
  check 'z   :=     2  * z  ; skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
