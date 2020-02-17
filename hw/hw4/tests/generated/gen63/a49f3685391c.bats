load ../../harness

@test "a49f3685391c" {
  check 'if (true     ∧true)   then 
skip  else  skip    ' '⇒ skip, {}'
}
