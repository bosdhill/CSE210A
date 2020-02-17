load ../../harness

@test "65adad2f67e9" {
  check 'skip    ;z     :=y  - z  ' '⇒ z := (y-z), {}
⇒ skip, {z → 0}'
}
