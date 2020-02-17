load ../../harness

@test "38baddbb205e" {
  check 'z   :=    1 +   d; 
z:=    -2     -   1 ' '⇒ skip; z := (-2-1), {z → 1}
⇒ z := (-2-1), {z → 1}
⇒ skip, {z → -3}'
}
