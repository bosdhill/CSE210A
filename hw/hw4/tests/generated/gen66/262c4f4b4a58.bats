load ../../harness

@test "262c4f4b4a58" {
  check 'while (¬(-3    +    N     <   z     +y))   do   skip' '⇒ skip, {}'
}
