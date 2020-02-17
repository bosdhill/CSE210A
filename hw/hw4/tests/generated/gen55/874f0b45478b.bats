load ../../harness

@test "874f0b45478b" {
  check 'if (true     ∨     4-    4    <-1+     z)     then    y     :=     3     +  re  else     skip' '⇒ y := (3+re), {}
⇒ skip, {y → 3}'
}
