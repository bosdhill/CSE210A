load ../../harness

@test "f28b13b7e8a1" {
  check 'if (¬true)     then  y   :=   y  *     x   else  
i8    :=2    *   sN    ' '⇒ i8 := (2*sN), {}
⇒ skip, {i8 → 0}'
}
