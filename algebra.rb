class Interval
	attr_reader :a, :b, :openA, :openB
	def initialize(a, b, openA, openB)
		if a < b
			@a = a
			@b = b
			@openA = openA
			@openB = openB
		else
			raise "Intervalo mal escrito Bobby."
		end
	end 
end

class Literal < Interval
	attr_reader :a, :b, :openA, :openB
	def initialize(a, b, openA, openB)
		super(a, b, openA, openB)
	end

	def to_s
		if openA
			ans = "("
		else
			ans = "["
		end
		ans += a.to_s
		ans += ","
		ans += b.to_s
		if openB
			ans += ")"
		else
			ans += "]"
		end
		ans
	end

	def intersection other
	end

	def union other
	end
end

class RightInfinite < Interval
	attr_reader :a, :b, :openA, :openB
	def initialize(a, openA)
		super(a, +(Float::INFINITY), openA, true)
	end

	def to_s
		if openA
			ans = "("
		else
			ans = "["
		end
		ans += a.to_s
		ans += ","
		ans += ")"
		ans
	end

	def intersection other
	end

	def union other
	end
end

class LeftInfinite < Interval
	attr_reader :a, :b, :openA, :openB
	def initialize(b, openB)
		super(-(Float::INFINITY), b, true, openB)
	end

	def to_s
		ans = "("
		ans += ","
		ans += b.to_s
		if openB
			ans += ")"
		else
			ans += "]"
		end
		ans
	end

	def intersection other
	end

	def union other
	end
end

class AllReals < Interval
	attr_reader :instancia, :a, :b, :openA, :openB
	@instancia = nil
	def initialize()
		if @instancia == nil
			@instancia = super(-(Float::INFINITY), +(Float::INFINITY), true, true)
		end
		@instancia
	end 

	def to_s
		ans = "(,)"
		ans
	end

	def intersection other
	end

	def union other
	end
end

class Empty < Interval
	def to_s
		ans = "empty"
		ans
	end

	def intersection other
	end

	def union other
	end
end
