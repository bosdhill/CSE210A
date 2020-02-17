load ../../harness

@test "fcacd8a7f741" {
  check 'x:=  -3   +     x     +    3' '⇒ skip, {x → 0}'
}
