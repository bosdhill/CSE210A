load ../../harness

@test "e722ec37b090" {
  check 'x  := z  +     y  ' '⇒ skip, {x → 0}'
}
