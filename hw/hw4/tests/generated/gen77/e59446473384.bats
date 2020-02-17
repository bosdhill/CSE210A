load ../../harness

@test "e59446473384" {
  check 'z    :=     z   -   3    ; 
  
z:=     -1  +3  ' '⇒ skip; z := (-1+3), {z → -3}
⇒ z := (-1+3), {z → -3}
⇒ skip, {z → 2}'
}
