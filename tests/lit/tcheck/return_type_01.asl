// RUN: not %asli --nobanner %s | %decolor | filecheck %s

func F(x : boolean) => integer
begin
    return x;
// CHECK: Type error: type integer does not match boolean
end
