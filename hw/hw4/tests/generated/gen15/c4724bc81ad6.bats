load ../../harness

@test "c4724bc81ad6" {
  check 'if (¬true)   then  
 z   := 4    -     2   else  z     :=4*     1' '⇒ z := (4*1), {}
⇒ skip, {z → 4}'
}
