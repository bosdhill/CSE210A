load ../../harness

@test "e408336b35bc" {
  check 'if (false ∨ false)      then  skip      else 
  skip     ' '⇒ skip, {}'
}
