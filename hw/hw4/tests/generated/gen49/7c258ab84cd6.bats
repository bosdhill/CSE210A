load ../../harness

@test "7c258ab84cd6" {
  check 'z    :=     4     * y   ; 

z :=y     - z ' '⇒ skip; z := (y-z), {z → 0}
⇒ z := (y-z), {z → 0}
⇒ skip, {z → 0}'
}
