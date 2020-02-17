load ../../harness

@test "ea790f049b84" {
  check 'if (-3 - z    =     z* y    ∧     false)   then 
x    := 0  *   -2  else 
  x    :=   -3  *    -3  ' '⇒ x := (-3*-3), {}
⇒ skip, {x → 9}'
}
