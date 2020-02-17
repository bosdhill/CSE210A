load ../../harness

@test "3da1780f2881" {
  check 'x   :=-3 -  x   ;y :=   -4+     x     ' '⇒ skip; y := (-4+x), {x → -3}
⇒ y := (-4+x), {x → -3}
⇒ skip, {x → -3, y → -7}'
}
