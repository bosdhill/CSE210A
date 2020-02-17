load ../../harness

@test "e3bc556d42bf" {
  check 'z    :=    -3    ;
 
 z:=  -1  - -1' '⇒ skip; z := (-1--1), {z → -3}
⇒ z := (-1--1), {z → -3}
⇒ skip, {z → 0}'
}
