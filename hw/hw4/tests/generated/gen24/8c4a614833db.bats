load ../../harness

@test "8c4a614833db" {
  check 'if (¬(0    *  x   <   y)) then 
z :=    z  -     z   else skip     ' '⇒ z := (z-z), {}
⇒ skip, {z → 0}'
}
