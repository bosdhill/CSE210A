load ../../harness

@test "9b72024011f3" {
  check 'if (¬(-4=x  +     3))      then   skip    else  z  :=    2    -   -3 ' '⇒ skip, {}'
}
