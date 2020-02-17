load ../../harness

@test "94b3c1de5eec" {
  check 'z:= 2  -t     ; 


x:=    x    -    z  ' '⇒ skip; x := (x-z), {z → 2}
⇒ x := (x-z), {z → 2}
⇒ skip, {x → -2, z → 2}'
}
