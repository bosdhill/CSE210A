load ../../harness

@test "bd23cbd32462" {
  check 'if (4-     1     =     3  +    0 ∧     x     *3    =    1     -    z   ∧   3     -   -2 = t +    1)     then 
skip   else  D     :=  E -   3    ' '⇒ D := (E-3), {}
⇒ skip, {D → -3}'
}
