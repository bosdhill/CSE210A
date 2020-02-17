load ../../harness

@test "897023c7e10c" {
  check 'if true     then   y    :=  3    else 
   skip   ' '⇒ y := 3, {}
⇒ skip, {y → 3}'
}
