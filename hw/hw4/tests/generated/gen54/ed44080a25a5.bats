load ../../harness

@test "ed44080a25a5" {
  check 'if (y  =    4--3  ∨    3 - 0  <x  -   z)  then 
 skip else   x     :=   2+   -4 ' '⇒ x := (2+-4), {}
⇒ skip, {x → -2}'
}
