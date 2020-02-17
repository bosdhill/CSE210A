load ../../harness

@test "f265e0ddb5df" {
  check 'x :=  K  +x     ' '⇒ skip, {x → 0}'
}
