// RUN: %asli --nobanner %s

type E of exception;

func T() => integer
begin
    throw E;
end

func F() => integer
begin
    return T()?;
end

