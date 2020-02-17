load ../../harness

@test "07d9e59bccba" {
  check 'y :=z     * a     ;z  :=   x    ' '⇒ skip; z := x, {y → 0}
⇒ z := x, {y → 0}
⇒ skip, {y → 0, z → 0}'
}
