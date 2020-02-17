load ../../harness

@test "7ebded9353bb" {
  check 'if (¬(fS     +   x <   y    +   -4)) then   
y:=    3 -   y      else 


y     := x*  -4    ' '⇒ y := (3-y), {}
⇒ skip, {y → 3}'
}
