load ../../harness

@test "03472faf2700" {
  check 'while (¬(O+  y =     x  + x)) do x   :=    2   +     x     ' '⇒ skip, {}'
}
