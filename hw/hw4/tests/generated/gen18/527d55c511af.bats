load ../../harness

@test "527d55c511af" {
  check 'while true   ∧2     - y  < 2    do  
y := f + 3' '⇒ skip, {}'
}
