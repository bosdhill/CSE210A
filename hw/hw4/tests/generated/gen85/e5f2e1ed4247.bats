load ../../harness

@test "e5f2e1ed4247" {
  check 'if (-3    *   3     <   -2    +  -1     ∨  1   *     3   <    Lb     +     -2)      then 
{skip;
  skip}     else 
   skip    ' '⇒ skip; skip, {}
⇒ skip, {}'
}
