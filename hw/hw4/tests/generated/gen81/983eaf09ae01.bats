load ../../harness

@test "983eaf09ae01" {
  check 'G :=     y     +     3;skip    ' '⇒ skip; skip, {G → 3}
⇒ skip, {G → 3}'
}
