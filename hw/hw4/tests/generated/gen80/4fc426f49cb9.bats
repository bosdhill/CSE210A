load ../../harness

@test "4fc426f49cb9" {
  check 'z   :=   x     --2 ;

 
z   :=     -1   ' '⇒ skip; z := -1, {z → 2}
⇒ z := -1, {z → 2}
⇒ skip, {z → -1}'
}
