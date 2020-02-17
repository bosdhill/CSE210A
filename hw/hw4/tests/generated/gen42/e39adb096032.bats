load ../../harness

@test "e39adb096032" {
  check 'x:=   -4    +  -2' '⇒ skip, {x → -6}'
}
