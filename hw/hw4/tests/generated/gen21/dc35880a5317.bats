load ../../harness

@test "dc35880a5317" {
  check 'if (¬true)   then 


x   :=-2  +  x    else    x  :=     z * 4  ' '⇒ x := (z*4), {}
⇒ skip, {x → 0}'
}
