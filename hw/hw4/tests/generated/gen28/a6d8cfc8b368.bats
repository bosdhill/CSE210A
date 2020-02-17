load ../../harness

@test "a6d8cfc8b368" {
  check 'if (x    *     -3 =    z  +    3∨    true)      then  
x :=x    +y     else  
 skip   ' '⇒ x := (x+y), {}
⇒ skip, {x → 0}'
}
