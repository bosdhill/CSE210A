load ../../harness

@test "1a85916e29bb" {
  check 'if (x     * z=    -1   -   y) then  y     :=    -2*y else z    :=  y     * z' '⇒ z := (y*z), {}
⇒ skip, {z → 0}'
}
