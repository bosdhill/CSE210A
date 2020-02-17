load ../../harness

@test "4c5cb85e6c4e" {
  check 'if false     then  z  :=    z      else z    :=  x   - -3     ' '⇒ z := (x--3), {}
⇒ skip, {z → 3}'
}
