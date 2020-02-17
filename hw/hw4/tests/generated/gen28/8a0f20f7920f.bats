load ../../harness

@test "8a0f20f7920f" {
  check 'if (y  +  -3=  -2    * z   ∨ d -   0   =  4     -    2)    then {skip   ;skip}      else    x   :=     y     +  4     ' '⇒ x := (y+4), {}
⇒ skip, {x → 4}'
}
