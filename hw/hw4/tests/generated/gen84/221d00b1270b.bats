load ../../harness

@test "221d00b1270b" {
  check 'if (z    +     x     =    3-     y ∨   true) then 

skip  else 

p     :=x    * 3' '⇒ skip, {}'
}
