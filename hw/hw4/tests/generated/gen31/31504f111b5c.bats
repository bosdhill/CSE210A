load ../../harness

@test "31504f111b5c" {
  check 'x     :=  2  * x   ' '⇒ skip, {x → 0}'
}
