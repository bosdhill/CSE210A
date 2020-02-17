load ../../harness

@test "9bb5e73e4205" {
  check 'if (x    <   4     *1     ∨    4  * t8  =   1*   x) then 
 d:=    x * 2 else 
 
x :=-1     -  -2 ' '⇒ d := (x*2), {}
⇒ skip, {d → 0}'
}
