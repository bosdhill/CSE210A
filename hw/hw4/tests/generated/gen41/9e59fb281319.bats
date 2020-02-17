load ../../harness

@test "9e59fb281319" {
  check 't1 :=  x*-3  ' '⇒ skip, {t1 → 0}'
}
