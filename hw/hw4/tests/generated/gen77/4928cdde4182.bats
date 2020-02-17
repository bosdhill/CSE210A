load ../../harness

@test "4928cdde4182" {
  check 'if (1   -   2<-2    --1∧ true)      then    
z     :=     -2 *     y     else z  :=    vU   *  2  ' '⇒ z := (vU*2), {}
⇒ skip, {z → 0}'
}
