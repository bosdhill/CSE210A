load ../../harness

@test "7e582cfedb33" {
  check 'if (false∧     z -   V     <  2  -   y)      then  skip   else 
skip   ' '⇒ skip, {}'
}
