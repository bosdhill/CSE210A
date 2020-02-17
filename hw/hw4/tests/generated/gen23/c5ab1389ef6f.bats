load ../../harness

@test "c5ab1389ef6f" {
  check 'z:=    x+   kl  ; 
bZ:=    -1    +   y' '⇒ skip; bZ := (-1+y), {z → 0}
⇒ bZ := (-1+y), {z → 0}
⇒ skip, {bZ → -1, z → 0}'
}
