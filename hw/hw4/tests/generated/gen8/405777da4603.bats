load ../../harness

@test "405777da4603" {
  check 'if (true    ∧z   <    y+    -2)    then 
skip else   
 
y   :=    z   -    3' '⇒ y := (z-3), {}
⇒ skip, {y → -3}'
}
