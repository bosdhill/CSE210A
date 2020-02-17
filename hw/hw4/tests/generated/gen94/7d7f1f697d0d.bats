load ../../harness

@test "7d7f1f697d0d" {
  check 'y  := z *  z;skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
