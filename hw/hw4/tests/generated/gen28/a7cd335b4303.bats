load ../../harness

@test "a7cd335b4303" {
  check 'if (1   -   1 <    -4* -4 ∨     true)     then  x    :=-4+     z else    z   := 0     -x  ' '⇒ x := (-4+z), {}
⇒ skip, {x → -4}'
}
