load ../../harness

@test "1b784edc4669" {
  check 'z    :=2    ;  
x:=  z+ -4   ' '⇒ skip; x := (z+-4), {z → 2}
⇒ x := (z+-4), {z → 2}
⇒ skip, {x → -2, z → 2}'
}
