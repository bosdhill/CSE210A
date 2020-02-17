load ../../harness

@test "755bc573fedb" {
  check 'if (¬(3     +   y  <  y))  then skip else  x :=  2   -  x     ' '⇒ skip, {}'
}
