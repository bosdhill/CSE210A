load ../../harness

@test "b0095f4dcc19" {
  check 'if (¬(y* 0     = 1    *     -3))      then 
iG   := y+  1 else  
  
y  :=-4' '⇒ iG := (y+1), {}
⇒ skip, {iG → 1}'
}
