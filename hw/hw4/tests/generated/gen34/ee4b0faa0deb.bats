load ../../harness

@test "ee4b0faa0deb" {
  check 'if (¬(-2*    qy <  -3+   z))      then 
y    :=3*    z      else  
z:=     0    +   3 ' '⇒ y := (3*z), {}
⇒ skip, {y → 0}'
}
