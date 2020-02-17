load ../../harness

@test "fc0c1295a9bf" {
  check 'if (¬(1- x   <     -3 *    aT))   then  skip      else  skip   ' '⇒ skip, {}'
}
