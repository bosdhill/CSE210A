load ../../harness

@test "adfe2ea891f2" {
  check 'if (-1- 1  < -3  +    b ∨  false)   then 
 skip   else skip    ' '⇒ skip, {}'
}
