load ../../harness

@test "aa30b7a17200" {
  check 'if (x     +   1  <y * x) then 

 y     :=-1    -  y      else skip  ' '⇒ skip, {}'
}
