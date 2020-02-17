load ../../harness

@test "809d641faa0f" {
  check 'skip   ; z:=    N  + z ' '⇒ z := (N+z), {}
⇒ skip, {z → 0}'
}
