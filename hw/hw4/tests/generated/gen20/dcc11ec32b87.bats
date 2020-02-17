load ../../harness

@test "dcc11ec32b87" {
  check 'if (ZH     -    2 =    1     -3 ∨     y    =     0   *    4)   then   l     :=    -2  -     hR else 
y   :=   Y     +     0   ' '⇒ l := (-2-hR), {}
⇒ skip, {l → -2}'
}
