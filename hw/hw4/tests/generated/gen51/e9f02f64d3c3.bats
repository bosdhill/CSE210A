load ../../harness

@test "e9f02f64d3c3" {
  check 'if (0     *  Hb  <  y   +z ∧   x-g=  x     *  3)      then skip    else  
 z :=    x   -     z ' '⇒ z := (x-z), {}
⇒ skip, {z → 0}'
}
