load ../../harness

@test "6854a1021cc3" {
  check 'if (z +-3     <  z   + c     ∧   x  -  -4  <   -4 *    Z)   then 

   skip   else  
c     :=    -3     *   y ' '⇒ c := (-3*y), {}
⇒ skip, {c → 0}'
}
