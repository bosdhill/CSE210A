load ../../harness

@test "019e8559c65f" {
  check 'z     :=zQ+  3    * z     ;y :=    x- z' '⇒ skip; y := (x-z), {z → 0}
⇒ y := (x-z), {z → 0}
⇒ skip, {y → 0, z → 0}'
}
