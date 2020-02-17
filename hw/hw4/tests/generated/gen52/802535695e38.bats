load ../../harness

@test "802535695e38" {
  check 'z:=  0   *  2;
   x     :=  -2+r    ' '⇒ skip; x := (-2+r), {z → 0}
⇒ x := (-2+r), {z → 0}
⇒ skip, {x → -2, z → 0}'
}
