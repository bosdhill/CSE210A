load ../../harness

@test "33a3e899cf0f" {
  check 'if (-3     *  y  =4     + g)     then 


y   :=   1    *     z     else 
y   :=    -3    *     -4     ' '⇒ y := (-3*-4), {}
⇒ skip, {y → 12}'
}
