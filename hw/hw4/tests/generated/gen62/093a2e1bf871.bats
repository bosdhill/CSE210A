load ../../harness

@test "093a2e1bf871" {
  check 'z:= -4   -   -3;skip' '⇒ skip; skip, {z → -1}
⇒ skip, {z → -1}'
}
