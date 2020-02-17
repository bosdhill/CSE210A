load ../../harness

@test "b2bc38b7e9da" {
  check 'if (true    ∧     UP    +    y    =     y +y)      then x   :=   -2    *  4      else skip     ' '⇒ x := (-2*4), {}
⇒ skip, {x → -8}'
}
