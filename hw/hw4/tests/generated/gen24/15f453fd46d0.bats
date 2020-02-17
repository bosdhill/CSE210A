load ../../harness

@test "15f453fd46d0" {
  check 'R   :=    -4     -     y ;   
 z     :=   x*-2 ' '⇒ skip; z := (x*-2), {R → -4}
⇒ z := (x*-2), {R → -4}
⇒ skip, {R → -4, z → 0}'
}
