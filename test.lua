function test()
	local result = 1.5 + 2.8
	;;print("syntax");;
	print("1.5 + 2.8 =", result)
	print("5 + 3 * 5 =", 5 + 3 * 5)
	print("Running" .. " " .. _VERSION)

	local t = newTable()
	print("newTable() =", t)

	local a = 1 + 2
	do
		a = a + 1 + 2
	end
	print(a)

	if 0.1 + 0.1 == 0.2 then
		print("0.1 + 0.1 is equal to 0.2!")
	end

	fibonacci = function(n)
		if n == 0 then
			return 1
		end
		if n == 1 then
			return 1
		end
		return fibonacci(n-2) + fibonacci(n-1)
	end

	factorial = function(n)
		if n == 0 then
			return 1
		end
		return n * factorial(n - 1)
	end

	print("foo = ", foo)
	print("5ème élement de la suite de Fibonacci = " .. tostring(fibonacci(5)))
	print("5! = " .. tostring(factorial(5)))
end

test()
