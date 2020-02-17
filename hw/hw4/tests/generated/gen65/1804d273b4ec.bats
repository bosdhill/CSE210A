load ../../harness

@test "1804d273b4ec" {
  check 'if (false ∧   true) then 




skip     else skip' '⇒ skip, {}'
}
