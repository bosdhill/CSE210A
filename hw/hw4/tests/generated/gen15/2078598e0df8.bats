load ../../harness

@test "2078598e0df8" {
  check 'skip   ;
  
x  := y  -     -4  ' '⇒ x := (y--4), {}
⇒ skip, {x → 4}'
}
