load ../../harness

@test "2d4a9efeb901" {
  check 'if (z  * x   <   -2     +   x  ∨     z--2     <  4+  z)    then 
 
HZ  :=   x     -    x    else skip' '⇒ HZ := (x-x), {}
⇒ skip, {HZ → 0}'
}
