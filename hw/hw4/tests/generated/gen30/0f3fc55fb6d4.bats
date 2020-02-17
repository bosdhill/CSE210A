load ../../harness

@test "0f3fc55fb6d4" {
  check 'y:=x  +   3;skip  ' '⇒ skip; skip, {y → 3}
⇒ skip, {y → 3}'
}
