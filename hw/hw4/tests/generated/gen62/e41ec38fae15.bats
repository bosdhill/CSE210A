load ../../harness

@test "e41ec38fae15" {
  check 'if (2     <   2    *  z∨     z +   e  <  m     -     x)    then 
 
 x    :=     2    +   4   else  z :=   3 * 0' '⇒ z := (3*0), {}
⇒ skip, {z → 0}'
}
