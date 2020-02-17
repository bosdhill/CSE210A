load ../../harness

@test "e6dd428c26e8" {
  check 'while (¬true)    do  
   skip  ' '⇒ skip, {}'
}
