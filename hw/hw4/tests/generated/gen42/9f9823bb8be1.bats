load ../../harness

@test "9f9823bb8be1" {
  check 'IC   :=     0   +     z;   y :=     4   -   -3     ' '⇒ skip; y := (4--3), {IC → 0}
⇒ y := (4--3), {IC → 0}
⇒ skip, {IC → 0, y → 7}'
}
