load ../../harness

@test "8df4c04e321d" {
  check 'if (¬(z  +     y    < 2 +    1))      then   skip     else    skip  ' '⇒ skip, {}'
}
