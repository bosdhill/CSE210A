load ../../harness

@test "dba1702b4d38" {
  check 'while y   <     x +   x   +z ∧   true      do skip    ' '⇒ skip, {}'
}
