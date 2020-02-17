load ../../harness

@test "94afb536cf00" {
  check 'z  :=   -3   +    z    ; 

 
y     :=     -3     -     -4' '⇒ skip; y := (-3--4), {z → -3}
⇒ y := (-3--4), {z → -3}
⇒ skip, {y → 1, z → -3}'
}
