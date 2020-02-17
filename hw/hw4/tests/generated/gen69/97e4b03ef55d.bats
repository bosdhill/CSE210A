load ../../harness

@test "97e4b03ef55d" {
  check 'if (-3     +    4=     z * x)    then  
 skip      else x:=  o4    -   -1 ' '⇒ x := (o4--1), {}
⇒ skip, {x → 1}'
}
