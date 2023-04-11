// RUN: not %asli --nobanner %s | %decolor | filecheck %s

config CFG : integer = 1;

func F()
begin
    CFG = 2;
// CHECK: Type error: assignment to immutable variable `CFG` declared at
end
