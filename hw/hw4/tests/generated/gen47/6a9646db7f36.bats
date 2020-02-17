load ../../harness

@test "6a9646db7f36" {
  check 'if (z     <-4 *  -2)      then 
y := y    -  -3   else skip    ' '⇒ y := (y--3), {}
⇒ skip, {y → 3}'
}
