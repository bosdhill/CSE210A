load ../../harness

@test "13fe07bc1062" {
  check 'if (x  -  -2 <y+ -2   ∨  x    + 1  =-4   -     z) then 

 y     :=    z     +     -4    else z :=     1-    z  ' '⇒ z := (1-z), {}
⇒ skip, {z → 1}'
}
