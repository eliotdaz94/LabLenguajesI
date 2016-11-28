#!/usr/bin/ruby
require 'singleton'


##
# Clase principal de los intervalos, encargada de ser la base de la calculadora
class Interval 
	
	##
	# Crea un intervalo el cual podra ser instanciado en las distintas subclases
	# mencionadas en el enunciado.
	#
	# Para las intersecciones de intervalos, se usaron las reglas basicas de ma
	# tematicas, en los metodos, se definen a,b,c y d como los elementos de los
	# intervalos a intersectar, (a,b) & (c,d) respectivamente, se verifica cual
	# esta incluido en cual mediante operaciones basicas de comparacion y se det
	# ermina si se debe o no crear un nuevo Intervalo o retornar alguno de los 2
	# utilizados.
	#
	# Para las uniones de intervalos, se usaron las mismas reglas y se procedio a
	# definir los mismos auxiliares a,b,c,d para determinar con operaciones basicas
	# de comparacion cual era el que abarcaba ambos intervalos y determinar si se de
	# bia o no crear un nuevo Intervalo.

	attr_accessor :LeftIncl, :RightIncl, :left, :right

	def initialize(li = false, ri = false, x, y)
		# Constructor de la clase, se encarga de verificar
		# si el intervalo a crear es valido, en caso de no serlo
		# arroja una excepcion.

		if(x > y) 
			raise "initialize: intervalo invalido"
		elsif((x == y && (!li || !ri)))
			Empty.instance()
		else
			@LeftIncl = li
			@RightIncl = ri
			@left = x
			@right = y
		end
	end

	def emptyInters other
		# Metodo para verificar si la interseccion entre 2 intervalos sera vacia.
		# En caso de que sea vacia retorna true, false en caso contrario.

		if(self.right < other.left)
			return true
		elsif(self.left > other.right)
			return true
		elsif (self.right == other.left && !(self.RightIncl && other.LeftIncl))
			return true
		elsif (self.left == other.right && !(self.LeftIncl && other.RightIncl))
			return true
		else
			return false
		end
	end

	def to_s
		# Metodo to_s el cual se encarga de representar el intervalo en forma de String.

		open = if self.LeftIncl then "[" else "("	end
		close = if self.RightIncl then "]" else ")" end
		return open + self.left.to_s + "," + self.right.to_s + close
	end

end

##
# Clase Literal, subclase de Interval la cual representa un intervalo de 2 numeros.

class Literal < Interval
	
	def initialize(li = false,ri = false,x = nil,y = nil)
		# Constructor de la clase.

		super(ri,li,x,y)
	end

	def intersection other
		# Metodo general para intersectar un Literal con otro intervalo.

		if(emptyInters other)
			return Empty.instance()
		else
			return other.intersectLit(self)
		end
	end

	def intersectLit other
		# Metodo para intersectar un Literal con otro, siguiendo las
		# reglas basicas de matematica para la interseccion de literales.

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
		# Metodo para intersectar un Literal con un RightInfinite.

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
		# Metodo para intersectar un Literal con un LeftInfinite.

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
		# Metodo para intersectar un Literal con un AllReals

		return self
	end

	def intersectEmpty other
		# Metodo para intersectar un Literal con un Empty.

		return other
	end

	def union other
		# Metodo general para unir un Literal con otro Interval.

		if(emptyInters other)
			raise "Interseccion vacia"
		else
			other.unionLit self
		end
	end

	def unionLit other
		# Metodo para unir un Literal con otro.

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
		# Metodo para unir un Literal con un RightInfinite.

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
		# Metodo para unir un Literal con un LeftInfinite.

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
		# Metodo para unir un Literal con un AllReals.

		return other
	end

	def unionEmpty other
		# Metodo para unir un Literal con un AllReals.
		return self
	end
end

##
# Clase RightInfinite, que representa un intervalo que llega hasta 
# el infinito positivo.

class RightInfinite < Interval
	
	def initialize(li = false,x = nil)
		# Constructor de la clase.

		super(li,false,x,Float::INFINITY)
	end

	def intersection other
		# Metodo general para intersectar un RightInfinite con otro intervalo.
		if(emptyInters other)
			return Empty.instance()
		else
			return other.intersectRight(self)
		end
	end

	def intersectLit other
		# Metodo para intersectar un RightInfinite con un Literal.

		a = self.left
		b = self.right
		c = other.left
		d = other.right

		if(c < a) then
			return Literal.new(self.LeftIncl, other.RightIncl, a, d)
		else
			return other
		end
	end

	def intersectRight other
		# Metodo para intersectar un RightInfinite con otro RightInfinite.

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
		# Metodo para intersectar un RightInfinite con un LeftInfinite.

		a = self.left
		b = self.right
		c = other.left
		d = other.right

		return Literal.new(self.LeftIncl, other.RightIncl, a, d)
	end

	def intersectAll other
		# Metodo para intersectar un RightInfinite con un AllReals.
		
		return self
	end

	def intersectEmpty other
		# Metodo para intersectar un RightInfinite con un Empty.

		return other
	end

	def union other
		# Metodo general para unir un RightInfinite con otro Intervalo.

		if(emptyInters other)
			raise("Interseccion vacia")
		else
			other.unionRight self
		end
	end

	def unionLit other
		# Metodo para unir un RightInfinite con un Literal.

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
		# Metodo para unir un RightInfinite con otro RightInfinite.

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
		# Metodo para unir un RightInfinite con un LeftInfinite.

		return AllReals.instance()
	end

	def unionAll other
		# Metodo para unir un RightInfinite con un AllReals.

		return other
	end

	def unionEmpty other
		# Metodo para unir un RightInfinite con un Empty.
		return self
	end

	def to_s
		# Metodo to_s que convierte el Intervalo en un String.

		open = if self.LeftIncl then "[" else "("	end
		close = if self.RightIncl then "]" else ")" end
		return open + self.left.to_s + "," + close
	end

end

##
# Clase LeftInfinite, usada para representar los numeros desde -infinito
# hasta algun otro numero.

class LeftInfinite < Interval
	
	def initialize(ri = false, y = nil)
		# Constructor de la clase. 

		super(false,ri,-Float::INFINITY,y)
	end

	def intersection other
		# Metodo general para intersectar un LeftInfinite con otro Intervalo.
		
		if(emptyInters other)
			return Empty.instance()
		else
			return other.intersectLeft(self)
		end
	end		

	def intersectLit other
		# Metodo para intersectar un LeftInfinite con un Literal.

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
		# Metodo para intersectar un LeftInfinite con un RightInfinite.

		a = self.left
		b = self.right
		c = other.left
		d = other.right
		return Literal.new(other.LeftIncl, self.RightIncl, c, b)
	end

	def intersectLeft other
		# Metodo para intersectar un LeftInfinite con otro LeftInfinite.

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
		# Metodo para intersectar un LeftInfinite con un AllReals.

		return self
	end

	def intersectEmpty other
		# Metodo para intersectar un LeftInfinite con un Empty.

		return other
	end

	def union other
		# Metodo general para unir un LeftInfinite con otro Intervalo.

		if(emptyInters other)
			raise "Interseccion vacia"
		else
			other.unionLeft self
		end
	end

	def unionLit other
		# Metodo para unir un LeftInfinite con un Literal.

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
		# Metodo para unir un LeftInfinite con un RightInfinite.

		return AllReals.instance()
	end

	def unionLeft other
		# Metodo para unir un LeftInfinite con otro LeftInfinite.

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
		# Metodo para unir un LeftInfinite con un AllReals.

		return other
	end

	def unionEmpty other
		# Metodo para unir un LeftInfinite con un Empty.

		return self
	end

	def to_s
		# Metodo to_s encargado de convertir el LeftInfinite en un String.

		open = if self.LeftIncl then "[" else "("	end
		close = if self.RightIncl then "]" else ")" end
		return open + "," + self.right.to_s + close
	end
end

##
# Clase encargada de representar todos los numeros reales como un intervalo.

class AllReals < Interval

	include Singleton

	def initialize
		# Constructor de la clase.

		super(false, false, -Float::INFINITY, Float::INFINITY)
	end

	def intersection other
		# Metodo para intersectar un AllReals con otro Intervalo.

		return other.intersectAll self
	end

	def intersectLit other
		# Metodo para intersectar un AllReals con un Literal.

		return other
	end

	def intersectRight other
		# Metodo para intersectar un AllReals con un RightInfinite.

		return other
	end

	def intersectLeft other
		# Metodo para intersectar un AllReals con un LeftInfinite.

		return other
	end

	def intersectAll other
		# Metodo para intersectar un AllReals con otro AllReals.

		return other
	end

	def intersectEmpty other
		# Metodo para intersectar un AllReals con un Empty.

		return other
	end

	def union other
		# Metodo general para unir un AllReals con otro Intervalo.

		return self
	end

	def unionLit other
		# Metodo para unir un AllReals con un Literal.

		return self
	end

	def unionRight other
		# Metodo para unir un AllReals con un RightInfinite.

		return self
	end

	def unionLeft other
		# Metodo para unir un AllReals con un LeftInfinite.

		return self
	end

	def unionAll other
		# Metodo para unir un AllReals con otro AllReals.

		return self
	end

	def unionEmpty other
		# Metodo para unir un AllReals con un Empty.

		return self
	end

	def to_s
		# Metodo que se encarga de escribir un AllReals como un String.

		return "(,)"
	end
end

##
# Clase Empty que representa el intervalo vacio como un singleton.

class Empty < Interval
	
	include Singleton

	def initialize
		# Constructor de la clase Empty.

		@includeLeft = false
		@includeRight = false
		@left = 0
		@right = 0
		$empty = self			
	end

	def intersection other
		# Metodo general para intersectar un Empty con otro Intervalo.

		return other.intersectEmpty self
	end

	def intersectLit other
		# Metodo para intersectar un Empty con un Literal.

		return self
	end

	def intersectRight other
		# Metodo para intersectar un Empty con un RightInfinite.

		return self
	end

	def intersectLeft other
		# Metodo para intersectar un Empty con un LeftInfinite.

		return self
	end

	def intersectAll other
		# Metodo para intersectar un Empty con un AllReals.

		return self
	end

	def intersectEmpty other
		# Metodo para intersectar un Empty con otro Empty.

		return self
	end

	def union other
		# Metodo general para unir un Empty con otro Intervalo.

		return other
	end

	def unionLit other
		# Metodo para unir un Empty con un Literal.

		return other
	end

	def unionRight other
		# Metodo para unir un Empty con un RightInfinite.

		return other
	end

	def unionLeft other
		# Metodo para unir un Empty con un LeftInfinite.

		return other
	end

	def unionAll other
		# Metodo para unir un Empty con un AllReals.
		
		return other
	end

	def unionEmpty other
		# Metodo para unir un Empty con un Empty.

		return other
	end

	def to_s
		# Metodo que representa un Empty como un String.

		return "empty"
	end	
end

def init_calculator(filename,variables)

	# Metodo que inicia la caluladora, abre el archivo de texto especificado
	# en el terminal el cual contiene las expresiones que iniciaran los inter
	# valos. Se utiliza un hash para saber cuales variables ya han sido inicia
	# das, en caso de no encontrarse, se inicia como un AllReals y se realizan
	# las operaciones indicadas en la expresion. Se revisa cual es la operacion
	# a realizar y luego se crea el Intervalo correspondiente. En caso de existir
	# un operador invalido, se reporta con un error en la ejecucion. 
	
	begin
		file = open(filename)
	rescue
		raise("Error: No existe el archivo " + filename + ".")
	end
	for line in file
		unionA = line.split("|")
		for u in unionA
			intersecA = u.split("&")
			var = nil
			aux = AllReals.instance()
			for i in intersecA
				j = i.split()
				if var == nil
					var = j[0]
				else
					if var != j[0]
						raise("Error: Operación de conjunción inválida en el archivo " + filename + ".")
					end
				end
				if j[1] == ">="
					interval = RightInfinite.new(true,j[2].to_i)
				elsif j[1] == "<="
					interval = LeftInfinite.new(true,j[2].to_i)
				elsif j[1] == ">"
					interval = RightInfinite.new(false,j[2].to_i)
				elsif j[1] == "<"
					interval = LeftInfinite.new(false,j[2].to_i)
				else 
					raise("Error: Operador inválido en el archivo " + filename + ".")
				end
				aux = aux.intersection(interval)
			end
			var = j[0]
			if variables.has_key?(var)
				variables[var] = variables[var].union(aux)
			else
				variables[var] = aux
			end
		end
	end
	variables.each_pair{|key,value| puts(key + " in " + value.to_s)}
end

if ARGV.length == 0
	puts("Error: No especificó el archivo de entrada.")
else
	variables = Hash.new()
	init_calculator(ARGV[0],variables)
	command = STDIN.gets()
	while(command != "exit\n")
		aux_line = command.split()
		if variables.has_key?(aux_line[0])
			if variables.has_key?(aux_line[2])
				if (aux_line[1] == "&")
					aux_interval = variables[aux_line[0]].intersection(variables[aux_line[2]])
					puts(aux_interval.to_s)
				elsif (aux_line[1] == "|")
					aux_interval = variables[aux_line[0]].union(variables[aux_line[2]])
					puts(aux_interval.to_s)
				else
					puts("Error: Operador inválido.")
				end
			else
				puts("Error: La variable " + aux_line[2] + " no existe.")	
			end
		else
			puts("Error: La variable " + aux_line[0] + " no existe.")
		end
		command = STDIN.gets()
	end
end

