load ../../harness

@test "7f750fac87c9" {
  check 'if (false∧true)   then 
y   := x    --2 else skip   ' '⇒ skip, {}'
}
