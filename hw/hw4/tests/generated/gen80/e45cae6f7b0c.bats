load ../../harness

@test "e45cae6f7b0c" {
  check 'if (z     < 2∨  false)     then     z   :=  2 else skip  ' '⇒ z := 2, {}
⇒ skip, {z → 2}'
}
