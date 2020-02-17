load ../../harness

@test "0407ec9b169d" {
  check 'skip;z  :=x    -     -3    ' '⇒ z := (x--3), {}
⇒ skip, {z → 3}'
}
