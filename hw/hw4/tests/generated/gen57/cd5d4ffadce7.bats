load ../../harness

@test "cd5d4ffadce7" {
  check 'YK:=    y     + x     ;y  :=  jl   -    1     ' '⇒ skip; y := (jl-1), {YK → 0}
⇒ y := (jl-1), {YK → 0}
⇒ skip, {YK → 0, y → -1}'
}
