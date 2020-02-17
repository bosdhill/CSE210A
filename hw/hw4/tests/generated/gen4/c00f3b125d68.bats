load ../../harness

@test "c00f3b125d68" {
  check 'if (2    *     y   =     3    +   -2∨  -1   +-1  <   4    *     3)    then  
skip     else 
 skip  ' '⇒ skip, {}'
}
