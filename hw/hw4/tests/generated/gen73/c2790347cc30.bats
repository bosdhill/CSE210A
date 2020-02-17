load ../../harness

@test "c2790347cc30" {
  check 'if (x-0  <   z*    z ∧ -3 -x =  z   +4)      then    
 z:=  2    *     Z  else x   :=     TS  *    x  ' '⇒ x := (TS*x), {}
⇒ skip, {x → 0}'
}
