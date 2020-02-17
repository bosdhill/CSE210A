load ../../harness

@test "d4bf9171f115" {
  check 'x:= W  * 4     ; 
   x    :=     x     *   0   ' '⇒ skip; x := (x*0), {x → 0}
⇒ x := (x*0), {x → 0}
⇒ skip, {x → 0}'
}
