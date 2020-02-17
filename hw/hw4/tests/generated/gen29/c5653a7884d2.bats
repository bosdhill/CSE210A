load ../../harness

@test "c5653a7884d2" {
  check 'if (3   *    0     =   y+  y    ∧  yE*-3    <z *     -1)  then y :=  3   +x      else    x   :=   2   +     z ' '⇒ x := (2+z), {}
⇒ skip, {x → 2}'
}
