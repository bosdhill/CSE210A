load ../../harness

@test "484c4ce9ab43" {
  check 'if (0-     0   = -3 *  -2∨  2   +     z     =   -2)     then  z:=z* hJ     else   
z:=    x   +z     ' '⇒ z := (x+z), {}
⇒ skip, {z → 0}'
}
