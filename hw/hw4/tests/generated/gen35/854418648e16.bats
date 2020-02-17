load ../../harness

@test "854418648e16" {
  check 'y  := z  + z+  -1;kA  :=    kr *   x  ' '⇒ skip; kA := (kr*x), {y → -1}
⇒ kA := (kr*x), {y → -1}
⇒ skip, {kA → 0, y → -1}'
}
