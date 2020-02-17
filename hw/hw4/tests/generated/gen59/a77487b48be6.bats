load ../../harness

@test "a77487b48be6" {
  check 'while (¬true)    do 
  skip     ' '⇒ skip, {}'
}
