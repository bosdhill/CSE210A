load ../../harness

@test "5688bf1e952b" {
  check 'y :=    y  *    x   ;skip ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
