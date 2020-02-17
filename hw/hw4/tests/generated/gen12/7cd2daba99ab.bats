load ../../harness

@test "7cd2daba99ab" {
  check 'DP  :=x    - x;
x :=3 *  0 ' '⇒ skip; x := (3*0), {DP → 0}
⇒ x := (3*0), {DP → 0}
⇒ skip, {DP → 0, x → 0}'
}
