// RUN: not %asli --nobanner %s | %decolor | filecheck %s

type E of exception;

func T() => integer
begin
    throw E;
end

func F() => integer
begin
    return T();
// CHECK: Type error: call to function `T.0` should be marked with `?` because it can throw an exception
end

