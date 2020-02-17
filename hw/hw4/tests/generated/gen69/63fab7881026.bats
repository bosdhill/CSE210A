load ../../harness

@test "63fab7881026" {
  check 'if (¬(3   *    3    =   y     +  x))    then  skip   else 
 skip     ' '⇒ skip, {}'
}
