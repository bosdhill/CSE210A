load ../../harness

@test "04189161d753" {
  check 'while true   ∧  0  * z=2+y∧  true     do x   :=   z     * z     ' '⇒ skip, {}'
}
