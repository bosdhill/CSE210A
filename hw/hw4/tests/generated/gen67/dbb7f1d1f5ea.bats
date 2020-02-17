load ../../harness

@test "dbb7f1d1f5ea" {
  check 'if (¬(z     <    p))   then    M9    :=   2    +   x     else skip  ' '⇒ M9 := (2+x), {}
⇒ skip, {M9 → 2}'
}
