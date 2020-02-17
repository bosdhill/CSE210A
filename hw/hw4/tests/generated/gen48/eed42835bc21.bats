load ../../harness

@test "eed42835bc21" {
  check 'if (¬(-1    =x    *    -3)) then {skip     ; t    :=  0 +    x}      else 
 skip ' '⇒ skip; t := (0+x), {}
⇒ t := (0+x), {}
⇒ skip, {t → 0}'
}
