load ../../harness

@test "01214d17ffbc" {
  check 'y   :=     x  +   0   ;

Z    :=  y    +    4  ' '⇒ skip; Z := (y+4), {y → 0}
⇒ Z := (y+4), {y → 0}
⇒ skip, {Z → 4, y → 0}'
}
