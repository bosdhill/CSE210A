load ../../harness

@test "830e294b3d64" {
  check 'if (¬(z   =    z   *-3))     then 
 
 z    :=     z    +    -2    else 
z    :=  1    * z  ' '⇒ z := (1*z), {}
⇒ skip, {z → 0}'
}
