load ../../harness

@test "cf234eaab434" {
  check 'while 3  --3    =   x     -     0    ∧   true    do 
 x :=x *z   ' '⇒ skip, {}'
}
