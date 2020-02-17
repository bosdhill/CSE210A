load ../../harness

@test "8fd776ab4172" {
  check 'if (4  -   0   <  -3     +     -4   ∨    -4  -  z  <  2+     z)    then 

x :=    y   *  -4      else skip  ' '⇒ x := (y*-4), {}
⇒ skip, {x → 0}'
}
