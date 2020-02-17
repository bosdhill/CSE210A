load ../../harness

@test "2045ff1ee893" {
  check 'z:=4    * x ;
 
 
x  :=    y -    z  ' '⇒ skip; x := (y-z), {z → 0}
⇒ x := (y-z), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
