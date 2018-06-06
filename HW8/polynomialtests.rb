# Klemen Kotar CSE341, Spring 2018, Section AA, Homework 8

require 'minitest/autorun'
load 'polynomials.rb'


class TestPolynomials < Minitest::Test

	def test_expressions
		# test if piece is flipped across 180 properly
		x = "x".asPolynomial
		assert_equal(x.to_s, "Polynomial(x)")

		y = "y".asPolynomial
		assert_equal(y.to_s, "Polynomial(y)")

		p1 = -10.asPolynomial
		assert_equal(p1.to_s, "Polynomial(-10)")

		p2 = 2*x*y +3
		assert_equal(p2.to_s, "Polynomial(2*x*y+3)")

		p3 = (x+3)*y
		assert_equal(p3.to_s, "Polynomial(x*y+3*y)")

		p4 = x-8
		assert_equal(p4.to_s, "Polynomial(x-8)")

		p5 = 2*x*y*x*x +3
		assert_equal(p5.to_s, "Polynomial(2*x**3*y+3)")

		p6 = (x+1)*(x-1)
		assert_equal(p6.to_s, "Polynomial(x**2-1***2)")

		p7 = (3*x+5)*0
		assert_equal(p7.to_s, "Polynomial(0)")

		p8 = 10*y+3*x
		assert_equal(p8.to_s, "Polynomial(3*x+10*y)")

		p9 =  2*x*y + x*3*y
		assert_equal(p9.to_s, "Polynomial(5*x*y)")

		p10 = 10*x*y + 1 + y*3*x*2 + 10
		assert_equal(p10.to_s, "Polynomial(16*x*y+11)")

		squid = "squid".asPolynomial
		assert_equal(squid.to_s, "Polynomial(squid)")


	end
end
