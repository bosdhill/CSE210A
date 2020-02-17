load ../../harness

@test "6113d5876bad" {
  check 'if (¬(y  <    4))    then y :=   -3-0      else 
 y :=   y  +  D2' '⇒ y := (y+D2), {}
⇒ skip, {y → 0}'
}
