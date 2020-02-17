load ../../harness

@test "e661f199aabc" {
  check 'skip    ; y   := -2    +x     ' '⇒ y := (-2+x), {}
⇒ skip, {y → -2}'
}
