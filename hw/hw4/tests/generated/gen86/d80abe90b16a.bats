load ../../harness

@test "d80abe90b16a" {
  check 'D8:= -1  +    x ;skip   ' '⇒ skip; skip, {D8 → -1}
⇒ skip, {D8 → -1}'
}
