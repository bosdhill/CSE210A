load ../../harness

@test "c1d9c4a2beb4" {
  check 'EV  :=  2; y  :=    4*    x    ' '⇒ skip; y := (4*x), {EV → 2}
⇒ y := (4*x), {EV → 2}
⇒ skip, {EV → 2, y → 0}'
}
