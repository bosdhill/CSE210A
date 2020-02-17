load ../../harness

@test "9382615feaed" {
  check 'z:= -1     + y ;
 
x   :=    1+ x ' '⇒ skip; x := (1+x), {z → -1}
⇒ x := (1+x), {z → -1}
⇒ skip, {x → 1, z → -1}'
}
