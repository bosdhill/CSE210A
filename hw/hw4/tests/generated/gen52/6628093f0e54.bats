load ../../harness

@test "6628093f0e54" {
  check 'y    :=   YY  +    z  ;

 
z    :=3 -   -2' '⇒ skip; z := (3--2), {y → 0}
⇒ z := (3--2), {y → 0}
⇒ skip, {y → 0, z → 5}'
}
