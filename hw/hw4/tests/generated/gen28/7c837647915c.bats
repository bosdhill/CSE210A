load ../../harness

@test "7c837647915c" {
  check 'if (y    -    Te  <  x  *1   ∧   y    * -2< WC)      then  
 skip else    St   :=   x  -     x     ' '⇒ St := (x-x), {}
⇒ skip, {St → 0}'
}
