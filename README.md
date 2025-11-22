# ミドリ (MIDORI)

## Introduction

Midori is a general-purpose programming language. It features static typing and garbage collection. The Midori compiler is written in C++ and compiles to bytecode for the Midori Virtual Machine (MVM). 
The MVM is written in C++ and is responsible for executing the bytecode. The MVM is also responsible for garbage collection.
<br><br>
This project is still in early development. The language is not yet *that* usable.

## Examples
### Pow(x, n)
```
fixed Pow = fn(fixed x : Float, fixed n : Int) : Float
{
	if (n < 0)
	{
		return Pow(1.0 / x, -n);
	}
	else
	{
		if (n == 0)
		{
			return 1.0;
		}
		else
		{
			if ((n & 1) == 1)
			{
				return x * Pow(x, n - 1);
			}
			else
			{
				fixed sub_result = Pow(x, n / 2);
				return sub_result * sub_result;
			}
		}
	}
};
```
### Church Numeral
```
fixed zero = fn(fixed f : (Int, Int) -> Int, fixed x : Int) : Int
{
    return x;
};

fixed one = fn(fixed f : (Int, Int) -> Int, fixed x : Int) : Int
{
    return f(x, 1); 
};

fixed two = fn(fixed f : (Int, Int) -> Int, fixed x : Int) : Int
{
    return f(f(x, 1), 1); 
};

fixed add = fn(fixed n : ((Int, Int) -> Int, Int) -> Int, 
                fixed m : ((Int, Int) -> Int, Int) -> Int) 
              : ((Int, Int) -> Int, Int) -> Int
{
    return fn(fixed f : (Int, Int) -> Int, fixed x : Int) : Int
    {
        return n(f, m(f, x));
    };
};

fixed church_to_number = fn(fixed c : ((Int, Int) -> Int, Int) -> Int) : Int
{
    return c(fn(fixed x : Int, fixed y : Int) : Int { return x + y; }, 0);
};


fixed main = fn() : Unit
{
	IO::PrintLine(church_to_number(add(one, two)) as Text); // prints out "3"
	return ();
};
```
### Man or Boy Test
```
fixed x = fn(fixed x : Int) : () -> Int
{
    return fn() : Int { return x; };
};

fixed A = fn(var k : Int, fixed x1 : () -> Int, fixed x2 : () -> Int, 
        fixed x3 : () -> Int, fixed x4 : () -> Int, fixed x5 : () -> Int) : Int
{
    fixed B = fn() : Int
    {
        k = k - 1;
        return A(k, B, x1, x2, x3, x4); 
    };
    return k > 0 ? B() : x4() + x5();
};

fixed main = fn() : Int
{
    IO::PrintLine(A(10, x(1), x(-1), x(-1), x(1), x(0)) as Text);   

    return 0;
};
```
### Sum Type and Product Type
```
struct S1
{
	k : Int,
	l : Float,
};

union U1
{
	A(Int),
	B,
	C(Float, Int, Text),
	D(U1),
	E(S1),
};

fixed main = fn() : Unit
{
	var complex = new U1::C(3.14, 42, "Complex case");

	switch(complex)
	{
		case U1::C(var Float, var num, var str):
			IO::PrintLine("Matching complex C with values: " ++ (frac as Text) ++ ", " ++ (num as Text) ++ ", " ++ str);
		default:
			IO::PrintLine("default case");
	}


	return ();
};
```

## Development

### Building Midori

Midori uses a three-tier build configuration system:

**Debug** - Full diagnostics with AST dumps, bytecode disassembly, and stack traces:
```bash
cmake --build out/Debug --config Debug
```

**Development** - Optimized build with compilation progress (recommended for testing):
```bash
cmake --build out/Development --config Development
```

**Release** - Maximum performance with minimal output:
```bash
cmake --build out/Release --config Release
```

Build the standard library for your configuration:
```bash
cmake --build out/Development --config Development --target MidoriStdLib
```

### Running Tests

Run all tests using the Development build:
```bash
python scripts/run_tests.py
```

Run a specific test (auto-shows detailed output):
```bash
python scripts/run_tests.py --test simple
python scripts/run_tests.py --test closure/simple.mdr
```

Run specific test categories or patterns:
```bash
python scripts/run_tests.py --category closure
python scripts/run_tests.py --pattern recursive
```

Create new tests:
```bash
python scripts/new_test.py closure/my_new_test
python scripts/new_test.py expression/failure/invalid_syntax --should-fail
```

For detailed testing documentation, see:
- [TESTING.md](TESTING.md) - Comprehensive testing guide
- [scripts/README.md](scripts/README.md) - Script usage and examples