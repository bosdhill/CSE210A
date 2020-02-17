load ../../harness

@test "cc918653793e" {
  check 'a   :=     1   ;
   aV  :=    2    +     x     ' '⇒ skip; aV := (2+x), {a → 1}
⇒ aV := (2+x), {a → 1}
⇒ skip, {a → 1, aV → 2}'
}
