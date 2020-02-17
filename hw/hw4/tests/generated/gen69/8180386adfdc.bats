load ../../harness

@test "8180386adfdc" {
  check 'if (-1    * -3     <   z    -    1) then 

z:=  Lz   +y     else 
y:=    1 *     -4    ' '⇒ y := (1*-4), {}
⇒ skip, {y → -4}'
}
