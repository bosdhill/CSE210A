load ../../harness

@test "1cc781b13140" {
  check 'z     :=     1  + -4 ;
n :=   x1  -     z    ' '⇒ skip; n := (x1-z), {z → -3}
⇒ n := (x1-z), {z → -3}
⇒ skip, {n → 3, z → -3}'
}
