load ../../harness

@test "cce52bdd6dae" {
  check 'z  := y  *     y     ' '⇒ skip, {z → 0}'
}
