load ../../harness

@test "1ca6d2592b41" {
  check 'x     :=     x *     y     ' '⇒ skip, {x → 0}'
}
