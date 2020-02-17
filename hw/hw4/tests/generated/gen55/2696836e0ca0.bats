load ../../harness

@test "2696836e0ca0" {
  check 'l   :=y     *z;skip     ' '⇒ skip; skip, {l → 0}
⇒ skip, {l → 0}'
}
