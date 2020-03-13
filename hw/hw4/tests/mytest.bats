load harness

@test "my-while-sub" {
  check 'while x=0 do x := x - 3' '⇒ x := (x-3); while (x=0) do { x := (x-3) }, {}
⇒ skip; while (x=0) do { x := (x-3) }, {x → -3}
⇒ while (x=0) do { x := (x-3) }, {x → -3}
⇒ skip, {x → -3}'
}

@test "my-seq" {
  check 'x:= 420; if 419 < x then x:=12 else z := 12' '⇒ skip; if (419<x) then { x := 12 } else { z := 12 }, {x → 420}
⇒ if (419<x) then { x := 12 } else { z := 12 }, {x → 420}
⇒ x := 12, {x → 420}
⇒ skip, {x → 12}'
}

@test "my-if-spaces" {
  check 'if 0<x  ∧  9 = 9 then x := 2 else x:= 5' '⇒ x := 5, {}
⇒ skip, {x → 5}'
}

@test "my-if-seq" {
  check 'if x=0 then x:= 1 else x:= 3; if y < 4 then x:= 1 else x:= 3 ' '⇒ x := 1, {}
⇒ skip, {x → 1}'
}

@test "my-if-assign" {
  check 'if x=0 then x:= 4 else x:= 3' '⇒ x := 4, {}
⇒ skip, {x → 4}'
}