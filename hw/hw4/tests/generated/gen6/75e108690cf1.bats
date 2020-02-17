load ../../harness

@test "75e108690cf1" {
  check 'if (¬(-4 -  y =     iq))     then  
   y :=   0   * y      else y    :=    2*    4' '⇒ y := (0*y), {}
⇒ skip, {y → 0}'
}
