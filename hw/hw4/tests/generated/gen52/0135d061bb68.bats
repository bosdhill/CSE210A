load ../../harness

@test "0135d061bb68" {
  check 'x     :=    1   -   -3    ;  
y     :=  -2   -     z  ' '⇒ skip; y := (-2-z), {x → 4}
⇒ y := (-2-z), {x → 4}
⇒ skip, {x → 4, y → -2}'
}
