load ../../harness

@test "13b9951d950e" {
  check 'y  := z +    z ; 
z  :=    -3    *     x   ' '⇒ skip; z := (-3*x), {y → 0}
⇒ z := (-3*x), {y → 0}
⇒ skip, {y → 0, z → 0}'
}
