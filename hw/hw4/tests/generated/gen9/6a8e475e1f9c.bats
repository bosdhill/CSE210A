load ../../harness

@test "6a8e475e1f9c" {
  check 'while (¬true) do skip     ' '⇒ skip, {}'
}
