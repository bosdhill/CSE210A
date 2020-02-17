load ../../harness

@test "66509c0385ac" {
  check 'x   :=   y   +     0    ;y     :=  -2 -  -2 ' '⇒ skip; y := (-2--2), {x → 0}
⇒ y := (-2--2), {x → 0}
⇒ skip, {x → 0, y → 0}'
}
