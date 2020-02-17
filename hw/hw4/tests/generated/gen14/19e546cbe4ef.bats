load ../../harness

@test "19e546cbe4ef" {
  check 'if (true    ∧    y*   -2< E)  then   C:=  -1 +   z else  skip    ' '⇒ skip, {}'
}
