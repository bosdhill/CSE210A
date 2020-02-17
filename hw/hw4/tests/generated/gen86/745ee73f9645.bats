load ../../harness

@test "745ee73f9645" {
  check 'y:=    z     +    Z   ;   
skip' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
