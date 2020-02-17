load ../../harness

@test "0707459e4877" {
  check 'if (2*  -2   =     4∧  true)  then  
x     :=   y     +     0  else skip    ' '⇒ skip, {}'
}
