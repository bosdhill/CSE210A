load ../../harness

@test "d7284b3b4823" {
  check 'x  :=2*  yl    ;hT     :=2   +  3 ' '⇒ skip; hT := (2+3), {x → 0}
⇒ hT := (2+3), {x → 0}
⇒ skip, {hT → 5, x → 0}'
}
