load ../../harness

@test "cce3871ee663" {
  check 'z  :=  x   *    y     ;
 

 x  :=z+     x ' '⇒ skip; x := (z+x), {z → 0}
⇒ x := (z+x), {z → 0}
⇒ skip, {x → 0, z → 0}'
}
