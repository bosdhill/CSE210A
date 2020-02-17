load ../../harness

@test "e7c5ac042c77" {
  check 'z:=     z  +    S;skip' '⇒ skip; skip, {z → 0}
⇒ skip, {z → 0}'
}
