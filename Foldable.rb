#!/usr/bin/ruby

module Plegable
	def null
		return self.foldr(true) {|x| false}
	end

	def foldr1 &block
		if self.null then
			raise "Estructura Vacia"
		else
			return self.foldr(nil) {|x,y| if y == nil then x else block.call(x,y) end}
		end
	end

	def length 
		return self.foldr(0) {|x,y| 1+y}
	end

	def all? &block
		return self.foldr(true) {|x,y| block.call(x) && y}
	end

	def any? &block
		return self.foldr(false) {|x,y| block.call(x) || y}
	end

	def to_arr
		return self.foldr([]) {|x,y| [x].concat(y)}
	end

	def elem? to_find
		self.any? {|x| x == to_find}
	end
end

class Array
	include Plegable

	def foldr e, &b
		aux = e
		#puts(aux)
		i = self.size
		while i > 0
			i -= 1
			aux = b.call(self[i],aux)
		end
		return aux
	end
end

a = [1,5,3]
#x  = a.foldr(0) {|a,b| a+b}
x  = a.foldr1 {|a,b| a+b}
puts(x)
#puts(a.to_s)
#puts(a.any? {|x| x > 0})
#l = [1,2,1,1,1,1].null()
#puts(l)
class Rose
	attr_accessor :elem, :children
	include Plegable
	def initialize elem, children = []
		@elem = elem
		@children = children
	end
	def add elem
		@children.push elem
		self
	end
	def copiar
		copy = Rose.new(self.elem)
		i = 0
		while i < self.children.size
			copy.add(self.children[i].copiar)
			i += 1
		end
		return copy 
	end
	def foldr e, &b
		aux = e
		#puts("Estoy en el arbol con elemento: " + self.elem.to_s)
		#puts("Aux es: " + aux.to_s)
		if self.children.empty? then
			return b.call(self.elem,aux)
		end
		i = self.children.size
		while i > 0
			i -= 1
			cola = self.children[i]
			aux = cola.foldr aux, &b
		end
		aux = b.call(self.elem,aux)
		return aux
	end
	def avg
		contador = 1
		a = self.foldr1 {|x,y| contador+=1; x+y} 
		#{|x,y| if (y.is_a? (self.elem.class)) then [0.00+x+y,2] else [x+y[0],1+y[1]] end}
		return Float(a)/contador
	end
end


r = Rose.new(1, [
Rose.new(2, [ Rose.new(4), Rose.new(5) ]),
Rose.new(3, [ Rose.new(6) ])
])

p = r.foldr1 {|x,y| x+y}
#puts(p)
puts(r.avg)
#r.avg
t = Rose.new(1, [ Rose.new(5, [ Rose.new(10), Rose.new(6) ]) ])
puts(t.avg)