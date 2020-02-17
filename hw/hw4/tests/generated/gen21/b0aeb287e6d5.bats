load ../../harness

@test "b0aeb287e6d5" {
  check 'x     :=  x *0    ' '⇒ skip, {x → 0}'
}
