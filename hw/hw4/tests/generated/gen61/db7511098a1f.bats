load ../../harness

@test "db7511098a1f" {
  check 'if (3   =  x - x    ∨     4  +   1   =     y     -    3)    then z  :=     -2   *  y else 
 skip    ' '⇒ skip, {}'
}
