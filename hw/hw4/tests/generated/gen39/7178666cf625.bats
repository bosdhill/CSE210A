load ../../harness

@test "7178666cf625" {
  check 'if (x  < y   -     -4)   then  

 z  :=   4  + -4 else y     :=   -4    -  Y' '⇒ z := (4+-4), {}
⇒ skip, {z → 0}'
}
