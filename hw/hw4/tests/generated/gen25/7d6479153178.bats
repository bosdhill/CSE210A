load ../../harness

@test "7d6479153178" {
  check 'if (3     - SJ  =     ld∧    -1   *  y   <    y     --4)      then z :=     3 else  y    :=    -1    ' '⇒ y := -1, {}
⇒ skip, {y → -1}'
}
