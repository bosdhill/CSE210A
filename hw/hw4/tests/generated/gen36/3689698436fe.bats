load ../../harness

@test "3689698436fe" {
  check 'if (true   ∨1 +   z  =0   +-1)     then    

z :=     z     *  -1   else   
 skip   ' '⇒ z := (z*-1), {}
⇒ skip, {z → 0}'
}
