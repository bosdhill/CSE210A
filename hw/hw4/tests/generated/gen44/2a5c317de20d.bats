load ../../harness

@test "2a5c317de20d" {
  check 'x:=x   -     x     ' '⇒ skip, {x → 0}'
}
