load ../../harness

@test "274e99239bea" {
  check 'z    :=-3 +   -4 ;  
 
x := 2   -  y    ' '⇒ skip; x := (2-y), {z → -7}
⇒ x := (2-y), {z → -7}
⇒ skip, {x → 2, z → -7}'
}
