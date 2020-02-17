load ../../harness

@test "bd639cbe4eae" {
  check 'while (¬(1    *  1   < 1  + 3))   do    y := y  + J' '⇒ skip, {}'
}
