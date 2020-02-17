load ../../harness

@test "784306e81962" {
  check 'if (true∨   0+ x=   y    +2)  then   skip  else  bQ   :=   z    + X   ' '⇒ skip, {}'
}
