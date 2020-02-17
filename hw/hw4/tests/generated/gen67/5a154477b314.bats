load ../../harness

@test "5a154477b314" {
  check 'y :=    y     *   y   ;
 
 
y  :=   y     -     4  ' '⇒ skip; y := (y-4), {y → 0}
⇒ y := (y-4), {y → 0}
⇒ skip, {y → -4}'
}
