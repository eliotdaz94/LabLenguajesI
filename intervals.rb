class Interval 
	attr_accessor :LeftIncl, :RightIncl, :left, :right

	def initialize(li = false, ri = false, x, y)
		if(x > y) 
			raise "initialize: intervalo invalido"
		end
		if((x == y && (!li || !ri)))
			Empty.new()
		else
			@LeftIncl = li
			@RightIncl = ri
			@left = x
			@right = y
		end
	end

	def emptyInters other
		if(self.right < other.left)
			return true
		elsif(self.left > other.right)
			return true
		#revisar estas 2 de abajo
		elsif (self.right == other.left && !(self.RightIncl && other.LeftIncl))
			return true
		elsif (self.left == other.right && !(self.LeftIncl && other.RightIncl))
			return true
		else
			return false
		end
	end

	def to_s
		open = if self.LeftIncl then "[" else "("	end
		close = if self.RightIncl then "]" else ")" end
		return open + self.left.to_s + ", " + self.right.to_s + close
	end

end

class Literal < Interval
	
	def initialize(li = false,ri = false,x = nil,y = nil)
		super(ri,li,x,y)
	end

# INTERSECTIONS
	def intersection other
		if(emptyInters other)
			return Empty.new()
		else
			return other.intersectLit(self)
		end
	end

	def intersectLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a <= c && c <= d && d <= b)
			return other
		elsif(c <= a && a <= b && b <= d)
			return self
		elsif(c <= a && a < d && d <= b)
			return Literal.new(self.LeftIncl, other.RightIncl, a, d)
		elsif(a <= c && c < b && b <= d)
			return Literal.new(other.LeftIncl, self.RightIncl, c, b)
		end
	end

	def intersectRight other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(c < a) then
			return self
		else
			return Literal.new(other.LeftIncl, self.RightIncl, c, b)
		end
	end

	def intersectLeft other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(b < d) then
			return self
		else
			return Literal.new(self.LeftIncl, other.RightIncl, a, d)
		end
	end

	def intersectAll other
		return self
	end

	def intersectEmpty other
		return other
	end

# UNIONS

	def union other
		if(emptyInters other)
			raise "Interseccion vacia"
		else
			other.unionLit self
		end
	end

	def unionLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a <= c && c <= d && d <= b)
			return self
		elsif(c <= a && a <= b && b <= d)
			return other
		elsif(c <= a && a < d && b <= d)
			return Literal.new(other.LeftIncl, self.RightIncl, c,b)
		elsif(a <= c && c < b && b <= d)
			return Literal.new(self.LeftIncl, other.RightIncl, a,d)
		end	
	end

	def unionRight other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a < c)
			return RightInfinite.new(self.LeftIncl,other.LeftIncl,a,d)
		elsif(c < a)
			return other
		else
			if(self.LeftIncl)
				return RightInfinite.new(self.LeftIncl,other.LeftIncl,a,d)
			else
				return other
			end
		end	
	end

	def unionLeft other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(b < d)
			return other
		elsif(d < b)
			return LeftInfinite.new(other.LeftIncl, self.RightIncl, c, b)
		else
			if(self.RightIncl)
				return LeftInfinite.new(other.LeftIncl, self.RightIncl, c, b)
			else
				return other
			end
		end
	end

	def unionAll other
		return other
	end

	def unionEmpty other
		return self
	end
end

class RightInfinite < Interval
	
	def initialize(li = false,x = nil)
		super(li,false,x,Float::INFINITY)
	end

# INTERSECTIONS
	def intersection other
		if(emptyInters other)
			return Empty.new()
		else
			return other.intersectRight(self)
		end
	end

	def intersectLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(c < a) then
			return Literal.new(self.LeftIncl, otherS.RightIncl, a, d)
		else
			return other
		end
	end

	def intersectRight other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a < c)
			return other
		elsif(c < a)
			return self
		else
			if(self.LeftIncl)
				return other
			else
				return self
			end
		end

	end

	def intersectLeft other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		return Literal.new(self.LeftIncl, other.RightIncl, a, d)
	end

	def intersectAll other
		return self
	end

	def intersectEmpty other
		return other
	end

# UNION
	def union other
		if(emptyInters other)
			raise("Interseccion vacia")
		else
			other.unionRight self
		end
	end

	def unionLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a< c)
			return self
		elsif(c < a)
			return RightInfinite.new(other.LeftIncl, self.RightIncl, c, b)
		else
			if(self.LeftIncl)
				return self
			else
				return RightInfinite.new(other.LeftIncl, self.RightIncl, c, b)
			end
		end
	end

	def unionRight other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(a < c)
			return self
		elsif(c < a)
			return other
		else
			if(self.LeftIncl)
				return self
			else
				return other
			end
		end
	end

	def unionLeft other
		return AllReals.new()
	end

	def unionAll other
		return other
	end

	def unionEmpty other
		return self
	end

end

class LeftInfinite < Interval
	
	def initialize(ri = false, y = nil)
		super(false,ri,-Float::INFINITY,y)
	end

# INTERSECTIONS
	def intersection other
		if(emptyInters other)
			return Empty.new()
		else
			return other.intersectLeft(self)
		end
	end		

	def intersectLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(b < d)
			return Literal.new(other.LeftIncl, self.RightIncl, c, b)
		else
			return other
		end
	end

	def intersectRight other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		return Literal.new(other.LeftIncl, self.RightIncl, b, c)
	end

	def intersectLeft other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(b < d)
			return self
		elsif(d < b)
			return other
		else
			if(self.RightIncl)
				return other
			else
				return self
			end
		end				
	end

	def intersectAll other
		return self
	end

	def intersectEmpty other
		return other
	end
# UNIONS

	def union other
		if(emptyInters other)
			raise "Interseccion vacia"
		else
			other.unionLeft self
		end
	end

	def unionLit other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(b  < d)
			return LeftInfinite.new(self.LeftIncl, other.RightIncl, a, d)
		elsif(d < b)
			return self
		else
			if(self.RightIncl)
				return self
			else
				return LeftInfinite.new(self.LeftIncl, other.RightIncl, a, d)
			end
		end
	end

	def unionRight other
		return AllReals.new()
	end

	def unionLeft other
		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(d < b)
			return self
		elsif(b < d)
			return other
		else
			if(self.RightIncl)
				return self
			else
				return other
			end
		end
	end
			
	def unionAll other
		return other
	end

	def unionEmpty other
		return self
	end

end

class AllReals < Interval

	def initialize
		if $all then
			$all
		else
			super(false, false, -Float::INFINITY, Float::INFINITY)
			$all = self
		end
	end

# INTERSECTIONS
	def intersection other
		return other.intersectAll self
	end

	def intersectLit other
		return other
	end

	def intersectRight other
		return other
	end

	def intersectLeft other
		return other
	end

	def intersectAll other
		return other
	end

	def intersectEmpty other
		return other
	end

# UNIONS
	def union other
		return self
	end

	def unionLit other
		return self
	end

	def unionRight other
		return self
	end

	def unionLeft other
		return self
	end

	def unionAll other
		return self
	end

	def unionEmpty other
		return self
	end

	def to_s
		return "(-Infinity,Infinity)"
	end
end

class Empty < Interval
	
	def initialize
		if $empty then
			$empty
		else
			@includeLeft = false
			@includeRight = false
			@left = 0
			@right = 0
			$empty = self			
		end
	end

# INTERSECTIONS
	def intersection other
		return other.intersectEmpty self
	end

	def intersectLit other
		return self
	end

	def intersectRight other
		return self
	end

	def intersectLeft other
		return self
	end

	def intersectAll other
		return self
	end

	def intersectEmpty other
		return self
	end

# UNIONS
	def union other
		return other
	end

	def unionLit other
		return other
	end

	def unionRight other
		return other
	end

	def unionLeft other
		return other
	end

	def unionAll other
		return other
	end

	def unionEmpty other
		return other
	end

	def to_s
		return "empty"
	end	
end
