load ../../harness

@test "29445f701f3a" {
  check 'kX   := S     ;
    skip' '⇒ skip; skip, {kX → 0}
⇒ skip, {kX → 0}'
}
