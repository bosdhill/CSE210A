load ../../harness

@test "22364dbc3fa8" {
  check 'skip   ;F   :=    2    +     -3 ' '⇒ F := (2+-3), {}
⇒ skip, {F → -1}'
}
