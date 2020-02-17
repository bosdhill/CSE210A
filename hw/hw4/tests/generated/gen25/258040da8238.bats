load ../../harness

@test "258040da8238" {
  check 'z     :=     x    +-1    ;z   :=    z    *iI  ' '⇒ skip; z := (z*iI), {z → -1}
⇒ z := (z*iI), {z → -1}
⇒ skip, {z → 0}'
}
