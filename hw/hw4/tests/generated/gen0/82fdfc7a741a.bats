load ../../harness

@test "82fdfc7a741a" {
  check 'if (-3  *    x     =   x    -  2     ∨   false)   then  
 skip     else   
 y     :=    tz   * y  ' '⇒ y := (tz*y), {}
⇒ skip, {y → 0}'
}
