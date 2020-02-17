load ../../harness

@test "91832d439f63" {
  check 'if (false∨    z    =-3    *     4)  then 
 skip  else   y  :=     4     * z    ' '⇒ y := (4*z), {}
⇒ skip, {y → 0}'
}
