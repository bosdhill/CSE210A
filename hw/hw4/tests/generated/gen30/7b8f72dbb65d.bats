load ../../harness

@test "7b8f72dbb65d" {
  check 'skip ;x:=y-    z   ' '⇒ x := (y-z), {}
⇒ skip, {x → 0}'
}
