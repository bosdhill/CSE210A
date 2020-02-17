load ../../harness

@test "3fbdf0da1b74" {
  check 'if (¬false)      then 
y    :=y--4     else  
 
skip' '⇒ y := (y--4), {}
⇒ skip, {y → 4}'
}
