load ../../harness

@test "60f01a2d010a" {
  check 'y:=x +   z     ;
 

x   := I - x  ' '⇒ skip; x := (I-x), {y → 0}
⇒ x := (I-x), {y → 0}
⇒ skip, {x → 0, y → 0}'
}
