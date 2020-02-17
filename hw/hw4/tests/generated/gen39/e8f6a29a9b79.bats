load ../../harness

@test "e8f6a29a9b79" {
  check 'if (¬(2     *   -2    =   -2  -     -1)) then skip    else  skip   ' '⇒ skip, {}'
}
