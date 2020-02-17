load ../../harness

@test "0a24b43ac2cf" {
  check 'z   :=  2     ;
 

x :=x   -    3' '⇒ skip; x := (x-3), {z → 2}
⇒ x := (x-3), {z → 2}
⇒ skip, {x → -3, z → 2}'
}
