load ../../harness

@test "316e38972142" {
  check 'y  :=    2   -    x     ; 
MO :=     -2   - 4    ' '⇒ skip; MO := (-2-4), {y → 2}
⇒ MO := (-2-4), {y → 2}
⇒ skip, {MO → -6, y → 2}'
}
