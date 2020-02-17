load ../../harness

@test "dac89d9ba391" {
  check 'if (w2   +x  <  x     -z    ∨   false) then  skip   else    skip' '⇒ skip, {}'
}
