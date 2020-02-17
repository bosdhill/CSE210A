load ../../harness

@test "710ecc1a4438" {
  check 'x:=y   -     2    ' '⇒ skip, {x → -2}'
}
