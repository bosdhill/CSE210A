load ../../harness

@test "ec7796f3ba71" {
  check 'if (4-   -4  =  V   -   -1     ∧ -2    *   -3     =    y     -    y) then 



skip   else  
y    :=  y  *   z   ' '⇒ y := (y*z), {}
⇒ skip, {y → 0}'
}
