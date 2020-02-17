load ../../harness

@test "fec11a039979" {
  check 'x  :=   y  -y ' '⇒ skip, {x → 0}'
}
