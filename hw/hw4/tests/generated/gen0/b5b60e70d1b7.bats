load ../../harness

@test "b5b60e70d1b7" {
  check 'z  :=    z     *    z    ;   
z  :=  -3-   3    ' '⇒ skip; z := (-3-3), {z → 0}
⇒ z := (-3-3), {z → 0}
⇒ skip, {z → -6}'
}
