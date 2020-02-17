load ../../harness

@test "fa71f7c550ea" {
  check 'if (0    -   y  =   y) then   
 cm    :=    4 +   z  else  
skip' '⇒ cm := (4+z), {}
⇒ skip, {cm → 4}'
}
