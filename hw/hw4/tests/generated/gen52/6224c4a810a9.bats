load ../../harness

@test "6224c4a810a9" {
  check 'if (-4   -s<   z*   -2   ∧     false) then  s   :=  y    -     y    else  
z    := y   +    vm  ' '⇒ z := (y+vm), {}
⇒ skip, {z → 0}'
}
