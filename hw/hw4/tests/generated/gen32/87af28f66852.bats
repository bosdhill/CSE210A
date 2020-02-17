load ../../harness

@test "87af28f66852" {
  check 'skip    ;vu    := y     + 0 ' '⇒ vu := (y+0), {}
⇒ skip, {vu → 0}'
}
