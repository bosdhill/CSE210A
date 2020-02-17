load ../../harness

@test "8e0b42f7f2e7" {
  check 'if (¬(1 -     2   <y     +     s))      then 
skip else 
 
 y:= x -  z    ' '⇒ y := (x-z), {}
⇒ skip, {y → 0}'
}
