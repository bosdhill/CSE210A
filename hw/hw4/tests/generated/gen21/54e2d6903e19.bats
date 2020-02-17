load ../../harness

@test "54e2d6903e19" {
  check 'if (y   +0     <  y)   then 

y  :=    x+ j9     else 
z    :=  y   *    2     ' '⇒ z := (y*2), {}
⇒ skip, {z → 0}'
}
