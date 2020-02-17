load ../../harness

@test "0233e380fca3" {
  check 'x:=x -     z   ;skip  ' '⇒ skip; skip, {x → 0}
⇒ skip, {x → 0}'
}
