load ../../harness

@test "9f4548ef31c7" {
  check 'if (¬(z     +  x  = y  - x))     then 
 skip else z:=  x     *  2     ' '⇒ z := (x*2), {}
⇒ skip, {z → 0}'
}
