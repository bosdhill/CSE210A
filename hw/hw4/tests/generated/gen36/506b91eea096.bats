load ../../harness

@test "506b91eea096" {
  check 'if (¬false)    then  
   z     :=    y  * rQ      else skip ' '⇒ z := (y*rQ), {}
⇒ skip, {z → 0}'
}
