load ../../harness

@test "83a2b0c96fd2" {
  check 'e    := JL     ; 
 
y   :=2+-4 ' '⇒ skip; y := (2+-4), {e → 0}
⇒ y := (2+-4), {e → 0}
⇒ skip, {e → 0, y → -2}'
}
