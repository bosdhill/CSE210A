load ../../harness

@test "313b84beabeb" {
  check 'while false    ∧   0  * 3<   0     *WM   do   skip  ' '⇒ skip, {}'
}
