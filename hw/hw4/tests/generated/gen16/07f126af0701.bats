load ../../harness

@test "07f126af0701" {
  check 'if (¬(s7*   y <y    *     1))   then skip    else z     :=   M2    ' '⇒ skip, {}'
}
