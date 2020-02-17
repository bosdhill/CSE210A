load ../../harness

@test "45924a8beaeb" {
  check 'if (-3  *   2    <3     +    -2∨  -2     --3     =     2   + -1)     then x   :=  x *  z     else    
 x    := z  +3  ' '⇒ x := (x*z), {}
⇒ skip, {x → 0}'
}
