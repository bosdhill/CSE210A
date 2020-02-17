load ../../harness

@test "6755cfcfd36d" {
  check 'u := 1 *    Wv   ;    y   :=  0*M    ' '⇒ skip; y := (0*M), {u → 0}
⇒ y := (0*M), {u → 0}
⇒ skip, {u → 0, y → 0}'
}
