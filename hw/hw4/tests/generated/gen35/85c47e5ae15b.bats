load ../../harness

@test "85c47e5ae15b" {
  check 'if (¬true)      then G5     :=   x   *    yZ  else skip  ' '⇒ skip, {}'
}
