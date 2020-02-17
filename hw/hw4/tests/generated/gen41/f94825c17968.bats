load ../../harness

@test "f94825c17968" {
  check 'while (¬(z  +     y   <    -1    +    4))     do   y    :=    3     -    x    ' '⇒ skip, {}'
}
