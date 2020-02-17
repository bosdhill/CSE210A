load ../../harness

@test "938f130cdba2" {
  check 'while -1     *  y  =   j3     *  z ∧  z* z  <    4   +   x  do   
 
z     :=   y     +-4    ' '⇒ z := (y+-4); while (((-1*y)=(j3*z))∧((z*z)<(4+x))) do { z := (y+-4) }, {}
⇒ skip; while (((-1*y)=(j3*z))∧((z*z)<(4+x))) do { z := (y+-4) }, {z → -4}
⇒ while (((-1*y)=(j3*z))∧((z*z)<(4+x))) do { z := (y+-4) }, {z → -4}
⇒ skip, {z → -4}'
}
