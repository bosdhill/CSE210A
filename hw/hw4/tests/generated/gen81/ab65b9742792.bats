load ../../harness

@test "ab65b9742792" {
  check 'x  :=    -1    *  1     ; 

n :=    -1  +     4' '⇒ skip; n := (-1+4), {x → -1}
⇒ n := (-1+4), {x → -1}
⇒ skip, {n → 3, x → -1}'
}
