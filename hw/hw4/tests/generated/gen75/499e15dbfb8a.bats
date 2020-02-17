load ../../harness

@test "499e15dbfb8a" {
  check 'skip;g  := z  + x     ' '⇒ g := (z+x), {}
⇒ skip, {g → 0}'
}
