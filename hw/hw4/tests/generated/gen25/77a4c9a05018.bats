load ../../harness

@test "77a4c9a05018" {
  check 'B    :=    -2   -     3; B    :=     x   -   0  ' '⇒ skip; B := (x-0), {B → -5}
⇒ B := (x-0), {B → -5}
⇒ skip, {B → 0}'
}
