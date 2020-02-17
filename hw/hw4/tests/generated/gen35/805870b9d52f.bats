load ../../harness

@test "805870b9d52f" {
  check 'if (¬(y     * 3 =     y))      then   

y :=y    *  -1 else 
skip' '⇒ skip, {}'
}
