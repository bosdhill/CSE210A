load ../../harness

@test "491a68c974ad" {
  check 'if (x    -   1   = 0 *x∧  -1    +  -3  =    3     *     2)   then  

 y:=    y +    4     else  k  :=  z  *  3  ' '⇒ k := (z*3), {}
⇒ skip, {k → 0}'
}
