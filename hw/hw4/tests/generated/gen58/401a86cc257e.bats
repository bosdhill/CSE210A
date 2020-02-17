load ../../harness

@test "401a86cc257e" {
  check 'while (¬(x   *1  <    2    +    0))   do y  :=1  +   G0 ' '⇒ skip, {}'
}
