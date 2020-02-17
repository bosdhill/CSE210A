load ../../harness

@test "0090682ed6a1" {
  check 'z     :=-3 *     3   ;   z  := n  -   x ' '⇒ skip; z := (n-x), {z → -9}
⇒ z := (n-x), {z → -9}
⇒ skip, {z → 0}'
}
