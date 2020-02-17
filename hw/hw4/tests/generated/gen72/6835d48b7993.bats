load ../../harness

@test "6835d48b7993" {
  check 'z    :=  y  ;  
 z:=    z     -    -2     ' '⇒ skip; z := (z--2), {z → 0}
⇒ z := (z--2), {z → 0}
⇒ skip, {z → 2}'
}
