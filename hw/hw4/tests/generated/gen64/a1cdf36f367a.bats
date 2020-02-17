load ../../harness

@test "a1cdf36f367a" {
  check 'skip     ; z:=     x + 1    ' '⇒ z := (x+1), {}
⇒ skip, {z → 1}'
}
