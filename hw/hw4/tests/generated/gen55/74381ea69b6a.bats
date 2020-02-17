load ../../harness

@test "74381ea69b6a" {
  check 'if (¬(-4     <     -2*   -2))   then   
skip      else 
  CV  :=  z     *    y   ' '⇒ CV := (z*y), {}
⇒ skip, {CV → 0}'
}
