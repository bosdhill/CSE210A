load ../../harness

@test "8403266332ed" {
  check 'while 2   +    -4 <     y   +     y ∧   false  do  skip     ' '⇒ skip, {}'
}
