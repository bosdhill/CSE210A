load ../../harness

@test "f34266774bd9" {
  check 'gq:=4 +   -3; skip   ' '⇒ skip; skip, {gq → 1}
⇒ skip, {gq → 1}'
}
