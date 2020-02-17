load ../../harness

@test "6aa6fa55da13" {
  check 'if (true     ∨    -1     -     2=  x +    -2)   then  
skip   else 
 skip     ' '⇒ skip, {}'
}
