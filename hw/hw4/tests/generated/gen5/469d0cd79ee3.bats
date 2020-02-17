load ../../harness

@test "469d0cd79ee3" {
  check 'y:= z    *    3     ; skip ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
