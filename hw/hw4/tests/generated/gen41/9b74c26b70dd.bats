load ../../harness

@test "9b74c26b70dd" {
  check 'if (1     *y  < 0  *  z) then y  :=   2    else   skip' '⇒ skip, {}'
}
