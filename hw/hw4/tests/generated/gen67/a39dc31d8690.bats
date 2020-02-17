load ../../harness

@test "a39dc31d8690" {
  check 'skip ;G     := x  *     z; z     := 3     +   x  ' '⇒ G := (x*z); z := (3+x), {}
⇒ skip; z := (3+x), {G → 0}
⇒ z := (3+x), {G → 0}
⇒ skip, {G → 0, z → 3}'
}
