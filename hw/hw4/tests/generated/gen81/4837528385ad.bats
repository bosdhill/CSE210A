load ../../harness

@test "4837528385ad" {
  check 'if (-2+   -1     <     x    +  y∨    3  + x   =  0 +-1)    then  



z   :=    z  *   -3      else     z :=  0   -   -1   ' '⇒ z := (z*-3), {}
⇒ skip, {z → 0}'
}
