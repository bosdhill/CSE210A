load ../../harness

@test "501a3f4cb3e9" {
  check 'if (-4    -z     =   P1   +  -3   ∧ Kp -     M=  -4)     then 
 x   :=     z     +  -1 else y :=-2  *   z ' '⇒ y := (-2*z), {}
⇒ skip, {y → 0}'
}
