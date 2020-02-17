load ../../harness

@test "fcaf8b3acf71" {
  check 'if (¬(0     -    DC    <  -2    *     y))  then 
 skip      else y := 3    *    y' '⇒ skip, {}'
}
