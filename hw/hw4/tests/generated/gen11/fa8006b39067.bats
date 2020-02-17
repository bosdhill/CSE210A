load ../../harness

@test "fa8006b39067" {
  check 'if (x    =     -2 *  z∧     true)  then 
skip   else skip ' '⇒ skip, {}'
}
