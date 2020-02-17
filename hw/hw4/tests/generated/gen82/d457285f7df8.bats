load ../../harness

@test "d457285f7df8" {
  check 'if true     then  
skip    else 
   skip' '⇒ skip, {}'
}
