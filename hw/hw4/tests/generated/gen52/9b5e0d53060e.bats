load ../../harness

@test "9b5e0d53060e" {
  check 'while (¬true)     do x :=    y     --2    ' '⇒ skip, {}'
}
