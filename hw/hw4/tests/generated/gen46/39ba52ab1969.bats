load ../../harness

@test "39ba52ab1969" {
  check 'if (true     ∨false)    then skip      else  skip' '⇒ skip, {}'
}
