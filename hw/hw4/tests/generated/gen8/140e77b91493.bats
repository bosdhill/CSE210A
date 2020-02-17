load ../../harness

@test "140e77b91493" {
  check 'v  :=  y     ;
 
y     := 2  + z   +2 ' '⇒ skip; y := ((2+z)+2), {v → 0}
⇒ y := ((2+z)+2), {v → 0}
⇒ skip, {v → 0, y → 4}'
}
