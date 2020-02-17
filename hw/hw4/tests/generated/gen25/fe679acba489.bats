load ../../harness

@test "fe679acba489" {
  check 'if (0*z  =     y    * 0   ∨ -1-z   =    z)    then 
skip   else 
y:= r-  z ' '⇒ skip, {}'
}
