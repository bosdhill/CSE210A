load ../../harness

@test "d4d50116a336" {
  check 'if (0  +   z    =     2 +    z   ∨  y     =  -2  +    e)     then skip      else 
   skip    ;

x     :=u     *   3' '⇒ skip; x := (u*3), {}
⇒ x := (u*3), {}
⇒ skip, {x → 0}'
}
