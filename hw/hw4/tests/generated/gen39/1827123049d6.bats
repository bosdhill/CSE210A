load ../../harness

@test "1827123049d6" {
  check 'if (-4  *  4    <   -3 *   y    ∨ x* 3  <y *     z)      then  
 z    :=    Am--1      else   
  skip  ' '⇒ z := (Am--1), {}
⇒ skip, {z → 1}'
}
