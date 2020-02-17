load ../../harness

@test "db576fe183c6" {
  check 'if (¬true)     then    
y     :=1   -   3    else skip' '⇒ skip, {}'
}
