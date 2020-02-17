load ../../harness

@test "625d8b72f226" {
  check 'if (true∧  2-    2    =-2   - 2)    then  
skip else y:=  z     *  -1 ' '⇒ y := (z*-1), {}
⇒ skip, {y → 0}'
}
