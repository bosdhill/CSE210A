load ../../harness

@test "cdd6f6982aa6" {
  check 'x     :=  hP + x' '⇒ skip, {x → 0}'
}
