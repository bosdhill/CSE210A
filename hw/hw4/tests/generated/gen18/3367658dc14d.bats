load ../../harness

@test "3367658dc14d" {
  check 'y   := 3  +     2 ' '⇒ skip, {y → 5}'
}
