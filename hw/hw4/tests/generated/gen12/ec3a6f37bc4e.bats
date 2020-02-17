load ../../harness

@test "ec3a6f37bc4e" {
  check 'if (x-     -1  < z   +     z ∨true)    then  
 z     :=  1+     -4    else skip ' '⇒ z := (1+-4), {}
⇒ skip, {z → -3}'
}
