load ../../harness

@test "1026d39f2d17" {
  check 'if (-1 +     2 =  y  * -2∨    c*  2     <    z* -2)      then 



 skip      else      y:=  w + y    ' '⇒ y := (w+y), {}
⇒ skip, {y → 0}'
}
