// RUN: not %asli --nobanner %s | %decolor | filecheck %s

var X : integer;

func WX() => integer
begin
    X = 1;
    return 0;
end

type E of exception;

func T() => integer
begin
    throw E;
end

func F() => integer
begin
    return WX() + T();
// CHECK: Type error: expression behaviour depends on evaluation order
end
