load ../../harness

@test "880778289142" {
  check 'while -3*     2    =  0 *  x     ∧     true  do  x:=  1  -   z ' '⇒ skip, {}'
}
