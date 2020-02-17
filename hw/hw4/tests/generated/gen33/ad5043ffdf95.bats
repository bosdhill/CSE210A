load ../../harness

@test "ad5043ffdf95" {
  check 'while x  -  y  =  Sx +   4  ∧     false      do  x:=  x     *     z   ' '⇒ skip, {}'
}
