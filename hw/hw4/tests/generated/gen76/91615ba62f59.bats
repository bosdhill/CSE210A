load ../../harness

@test "91615ba62f59" {
  check 'if (¬true)      then 
x     :=3    +-1      else O     :=    z + -3   ' '⇒ O := (z+-3), {}
⇒ skip, {O → -3}'
}
