load ../../harness

@test "ca912000a477" {
  check 'y :=   2 +    x;

 
x     := y     -   -4    ' '⇒ skip; x := (y--4), {y → 2}
⇒ x := (y--4), {y → 2}
⇒ skip, {x → 6, y → 2}'
}
