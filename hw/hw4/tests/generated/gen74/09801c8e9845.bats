load ../../harness

@test "09801c8e9845" {
  check 'if (c     +y   = -1  *     2     ∨   z     =    z)  then 
 
x     :=  y     *   -3     else 
  
skip    ' '⇒ x := (y*-3), {}
⇒ skip, {x → 0}'
}
