load ../../harness

@test "5ca554475514" {
  check 'y     :=  z     +    y   ; skip  ' '⇒ skip; skip, {y → 0}
⇒ skip, {y → 0}'
}
