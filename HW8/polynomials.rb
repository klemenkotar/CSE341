# Klemen Kotar CSE341, Spring 2018, Section AA, Homework 8

# Class Polynomial is used to profeorm standard mathematical polynomial computations with numbers and variables
class Polynomial

	# creates new instance of Polynomial, with two optional parameters which default to "" and 0
	def initialize(var = "", coeff = 0)
		@terms = { {"" =>1} => 0, {var => 1} => coeff }
		@terms.default = 0
	end

	# adds another polynomial to this one
	def + other
		if other.respond_to?("asPolynomial") then other = other.asPolynomial end
		result = Polynomial.new
		result.terms = @terms.merge(other.terms){|term, coeff1, coeff2| coeff1 + coeff2}
		result.clean
		return result
	end

	# returns a new polynomials which results from adding another polynomial to this one
	def - other
		if other.respond_to?("asPolynomial") then other = other.asPolynomial end
		result = Polynomial.new
		result.terms = @terms.merge(other.terms){|term, coeff1, coeff2| coeff1 - coeff2}
		result.clean
		return result
	end

	# returns a new polynomials which results from subtracting another polynomial to this one
	def * other
		if other.respond_to?("asPolynomial") then other = other.asPolynomial end
		result = Polynomial.new # init result hash
		@terms.each do |term1, coeff1|
			other.terms.each do |term2, coeff2|
				newTerm = term1.merge(term2) { |var, exp1, exp2| exp1 + exp2 }
				result.terms[newTerm] += coeff1 * coeff2
			end
		end
		result.clean
		return result # return resultant terms of multiplication
	end

	def ** exp
		result = self # init result hash
		[0..exp].each { |x| result = self*self}
		return result
	end

	# returns a new polynomials which results from multiplying another polynomial with this one
	def to_s
		if @terms.empty? # print 0 for empty polynomial
			result = "0"
		else
			clean_terms = @terms.reject {|term, coeff| coeff == 0} # clean 0's from strings
			sorted_terms = clean_terms.to_a.sort do |this, other| 
				if this[0].keys == [""]
					1
				elsif other[0].keys == [""]
					-1
				elsif this[0].values.max == other[0].values.max
					this[0].keys.min <=> other[0].keys.min
				else
					this[0].values.max <=> other[0].values.max
				end
			end
			result = ""
			sorted_terms.each do |term|
				term_list = []
				# format string to follow mathematical convention
				term[0].each do |var, exponent|
				    term_list.push("#{var}#{exponent > 1 ? '**'+exponent.to_s : ''}" )
				end
				result += "#{term[1] > 0 ? '+' : '' }#{if term[1] == 1 then "" 
							elsif term_list.empty? or term_list == [''] then term[1] else term[1].to_s+'*' end}#{term_list.sort.join("*")}"
			end
		end
		if result.start_with?("+") then result = result[1..-1] end #trim leading + signs
		return "Polynomial(#{result})" 
	end

	# coerces obejct to become polynomial
	def coerce(n)
		[n.asPolynomial, self]
	end


	protected
		# protected acessor methode for the the terms hash
		attr_accessor :terms

		# protected helper methode that removes terms with a 0 coefficient
		def clean
			@terms = @terms.delete_if {|term, coeff| coeff == 0}
			sorted_terms = {}
			@terms.each { |term, coefficient| sorted_terms[term.sort.to_h] = coefficient }
			@terms = sorted_terms
			@terms = @terms.each {|term, coeff| if term.size > 1 then term.delete("") end }
		end
end

class Numeric
	# returns a number to a polynomial without a variable
	def asPolynomial
		Polynomial.new("", self)
	end
end

class String
	# converts
	def asPolynomial
		Polynomial.new(self, 1)
	end
end
