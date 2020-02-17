load ../../harness

@test "8896cc2b74cf" {
  check 'if (x + 2= 4     +    -4  ∧   z   *  -4     <    y    *   y)     then  skip   else 
 skip   ' '⇒ skip, {}'
}
