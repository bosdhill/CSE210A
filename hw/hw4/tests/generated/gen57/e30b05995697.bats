load ../../harness

@test "e30b05995697" {
  check 'if (¬(3  +  -4=   2  *    -2)) then skip      else  

z  :=    z    *   -2' '⇒ skip, {}'
}
