load ../../harness

@test "ca63471c0687" {
  check 'if (¬true) then 
x     :=    z +     x  else skip   ' '⇒ skip, {}'
}
