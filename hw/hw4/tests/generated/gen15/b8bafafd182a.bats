load ../../harness

@test "b8bafafd182a" {
  check 'x := -4     +     j     ;y     :=  -2 +  x  ' '⇒ skip; y := (-2+x), {x → -4}
⇒ y := (-2+x), {x → -4}
⇒ skip, {x → -4, y → -6}'
}
