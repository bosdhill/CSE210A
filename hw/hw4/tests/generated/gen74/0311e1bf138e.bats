load ../../harness

@test "0311e1bf138e" {
  check 'z     := 1     -    y ; 
 x:= y   *4  ' '⇒ skip; x := (y*4), {z → 1}
⇒ x := (y*4), {z → 1}
⇒ skip, {x → 0, z → 1}'
}
