load ../../harness

@test "f4385e505d89" {
  check 'if (¬(-3   *     -2     =s    + y))      then skip    else skip' '⇒ skip, {}'
}
