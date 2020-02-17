load ../../harness

@test "01704d1e2974" {
  check 'if (¬true)     then     
x  :=  z     +     z  else   skip  ' '⇒ skip, {}'
}
