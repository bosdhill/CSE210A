load ../../harness

@test "21a7809e6500" {
  check 'if (¬(-3  -    y < 4*   1))    then    skip else skip  ' '⇒ skip, {}'
}
