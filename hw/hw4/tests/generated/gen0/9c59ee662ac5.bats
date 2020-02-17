load ../../harness

@test "9c59ee662ac5" {
  check 'y   :=   z ;   skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
