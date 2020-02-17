load ../../harness

@test "6a6522d697ae" {
  check 'y:=     -1 + z ;y :=  e  -    -2' '⇒ skip; y := (e--2), {y → -1}
⇒ y := (e--2), {y → -1}
⇒ skip, {y → 2}'
}
