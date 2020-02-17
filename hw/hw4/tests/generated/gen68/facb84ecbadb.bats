load ../../harness

@test "facb84ecbadb" {
  check 'z    :=  -4    -   XM  ; 
RH   :=    2    -   -2' '⇒ skip; RH := (2--2), {z → -4}
⇒ RH := (2--2), {z → -4}
⇒ skip, {RH → 4, z → -4}'
}
