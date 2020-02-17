load ../../harness

@test "f416144c6b9c" {
  check 'if (¬(-3    +2=y    - y))    then  skip  else skip ' '⇒ skip, {}'
}
