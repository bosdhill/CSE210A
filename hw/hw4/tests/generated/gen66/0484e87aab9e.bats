load ../../harness

@test "0484e87aab9e" {
  check 'if (false    ∧    0 --1<  4     *  y)  then 

 skip   else   z := y    +  -1 ;

 

x  :=  3     +     x    ' '⇒ z := (y+-1); x := (3+x), {}
⇒ skip; x := (3+x), {z → -1}
⇒ x := (3+x), {z → -1}
⇒ skip, {x → 3, z → -1}'
}
