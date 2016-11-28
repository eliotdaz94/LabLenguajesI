#!/usr/bin/ruby

## 
# Módulo que implanta una infraestructura de extensión del comportamiento de 
# pliegue con la técnica de mixins. 

module Plegable

	##
	# Indica si la estructura de datos está vacía (true) o contiene elementos
	# (false).

	def null?
		return self.foldr(true) {|x| false}
	end

	##
	# Recorre la estructura de manera similar a foldr, pero usando el primer 
	# elemento como el caso base y hace el recorrido sobre el resto de la 
	# estructura. Si al estructura se encuentra vacía se lanza una excepción.

	def foldr1 &block
		if self.null then
			raise "Estructura Vacia"
		else
			return self.foldr(nil) {|x,y| if y == nil then x else block.call(x,y) end}
		end
	end

	##
	# Indica la cantidad de elementos que posee una estructura.

	def length 
		return self.foldr(0) {|x,y| 1+y}
	end

	##
	# Indica si todos los elementos de una estructura cumplen con un predicado
	# dado.

	def all? &block
		return self.foldr(true) {|x,y| block.call(x) && y}
	end

	##
	# Indica si existe algún elemento de la estructura que cumple con un 
	# predicado dado.

	def any? &block
		return self.foldr(false) {|x,y| block.call(x) || y}
	end

	##
	# Construye una nueva instancia de la clase Array con todos los elementos
	# contenidos, en orden de izquierda a derecha. 

	def to_arr
		return self.foldr([]) {|x,y| [x].concat(y)}
	end

	##
	# Indica si un elemento se encuentra en la estructura, haciendo las 
	# comparaciones mediante '=='.

	def elem? to_find
		self.any? {|x| x == to_find}
	end
end

##
# Se realiza la reapertura de la clase Array nativa de Ruby, para agregar el 
# método foldr que permite plegar la lista de derecha a izquierda y las 
# funcionalidades pertenecientes al mixin (o módulo) Plegable.

class Array

	include Plegable

	## 
	# Recibe un caso base y un bloque, que es utilizado para recorrer la lista
	# haciendo un pliegue de derecha a izquierda aplicando el bloque recibido.

	def foldr e, &b
		aux = e
		i = self.size
		while i > 0
			i -= 1
			aux = b.call(self[i],aux)
		end
		return aux
	end
end

##
# Clase que representa árboles multicaminos (Rose Trees) e incluye las 
# funcionalidades pertenecientes al mixin (o módulo) Plegable.

class Rose

	attr_accessor :elem, :children
	include Plegable

	##
	# Crea un nuevo árbol multicamino (Rose Tree) con valor 'elem' para su nodo
	# raíz y la lista vacía 'children' para almacenar los posibles múltiples
	# hijos.

	def initialize elem, children = []
		@elem = elem
		@children = children
	end

	##
	# Añade un nuevo hijo 'elem' al árbol multicamino.

	def add elem
		@children.push elem
		self
	end

	## 
	# Recibe un caso base y un bloque, que es utilizado para recorrer el árbol
	# haciendo un pliegue de derecha a izquierda aplicando el bloque recibido.

	def foldr e, &b
		aux = e
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

	##
	# Calcula el promedio de los valores en el árbol multicaminos usando
	# únicamente las funciones perteneciente al mixin 'Plegable', y 
	# recorriendo dicha estructura una sola vez para los cálculos necesarios.

	def avg
		contador = 1
		a = self.foldr1 {|x,y| contador+=1; x+y} 
		return Float(a)/contador
	end
end