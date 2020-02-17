load ../../harness

@test "af5728a45bad" {
  check 'while (¬true)    do 
skip    ' '⇒ skip, {}'
}
