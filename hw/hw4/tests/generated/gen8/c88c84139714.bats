load ../../harness

@test "c88c84139714" {
  check 'if (true     ∨x   +z     =     x -     x)    then 
  skip      else      skip' '⇒ skip, {}'
}
