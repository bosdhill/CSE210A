load ../../harness

@test "8fc74ed252a3" {
  check 'if (¬(y =     1  *    z))  then skip      else skip  ' '⇒ skip, {}'
}
