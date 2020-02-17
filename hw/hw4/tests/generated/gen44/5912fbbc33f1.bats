load ../../harness

@test "5912fbbc33f1" {
  check 'if (¬(N  -   -1    <   x  -  y))      then 
 skip else y    := -3  *  -4   ' '⇒ skip, {}'
}
