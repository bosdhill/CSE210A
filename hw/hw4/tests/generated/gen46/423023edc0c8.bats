load ../../harness

@test "423023edc0c8" {
  check 'if (-4+   z =    x    *    0     ∧     true)    then   
skip      else 
  X:=  z   ' '⇒ X := z, {}
⇒ skip, {X → 0}'
}
