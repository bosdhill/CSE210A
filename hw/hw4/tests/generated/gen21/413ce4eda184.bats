load ../../harness

@test "413ce4eda184" {
  check 'while (¬(x   =    y     *  y))    do  skip     ' '⇒ skip, {}'
}
