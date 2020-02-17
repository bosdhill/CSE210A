load ../../harness

@test "88fb16b7223b" {
  check 'if (true     ∨ -1  +     -4   <  -4   +     0)     then skip   else x   :=   x   +   y   ' '⇒ skip, {}'
}
