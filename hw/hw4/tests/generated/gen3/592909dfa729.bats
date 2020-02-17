load ../../harness

@test "592909dfa729" {
  check 'm    :=    x  *  y     ;skip' '⇒ skip; skip, {m → 0}
⇒ skip, {m → 0}'
}
