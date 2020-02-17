load ../../harness

@test "1a5edb2cdb74" {
  check 'if false      then skip      else z   :=     3   +S     ' '⇒ z := (3+S), {}
⇒ skip, {z → 3}'
}
