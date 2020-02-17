load ../../harness

@test "8a0e7f4b6196" {
  check 'z  := y *y ;

skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
