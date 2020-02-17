load ../../harness

@test "23636d806ba5" {
  check 'while (¬true)    do 
skip ' '⇒ skip, {}'
}
