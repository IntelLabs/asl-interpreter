// RUN: not %asli --nobanner %s | %decolor | filecheck %s

type E of exception;

func T()
begin
    throw E;
end

func F()
begin
    T();
// CHECK: Type error: call to procedure `T.0` should be marked with `?` because it can throw an exception
end
