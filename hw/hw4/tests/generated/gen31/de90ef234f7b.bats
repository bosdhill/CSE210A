load ../../harness

@test "de90ef234f7b" {
  check 'if (true    ∧    2+ x    <    3    *-1)      then 



x   :=    -2   +  z    else u  :=    2    ' '⇒ u := 2, {}
⇒ skip, {u → 2}'
}
