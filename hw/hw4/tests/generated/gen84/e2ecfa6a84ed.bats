load ../../harness

@test "e2ecfa6a84ed" {
  check 'z:= x    -    z     ;skip     ' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
