load ../../harness

@test "e0bee3098289" {
  check 'y:=   -2*  -1  ; 
z     :=     -1   -     z ' '⇒ skip; z := (-1-z), {y → 2}
⇒ z := (-1-z), {y → 2}
⇒ skip, {y → 2, z → -1}'
}
