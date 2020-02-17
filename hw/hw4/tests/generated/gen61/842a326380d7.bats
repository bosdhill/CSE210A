load ../../harness

@test "842a326380d7" {
  check 'if (x    +  3=  -3   *    x ∧0  -     -2    =-4  +     4)  then  o    :=   3    else 
 x :=-1*   -1 ' '⇒ x := (-1*-1), {}
⇒ skip, {x → 1}'
}
