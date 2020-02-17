load ../../harness

@test "0bf0481f0f72" {
  check 'if (-3  -   Z  =   x    +     3    ∧ bS   -    z<   2  * y)    then 
x    := z     else   
 y    :=   1 +   x ' '⇒ y := (1+x), {}
⇒ skip, {y → 1}'
}
