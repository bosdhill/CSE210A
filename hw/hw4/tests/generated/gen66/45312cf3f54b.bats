load ../../harness

@test "45312cf3f54b" {
  check 'while (¬(z     *    -2     <    2 +     z)) do skip ' '⇒ skip, {}'
}
