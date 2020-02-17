load ../../harness

@test "24d32b855d7e" {
  check 'if (true     ∨  false)   then 
skip else 
  skip     ' '⇒ skip, {}'
}
