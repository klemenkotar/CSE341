# University of Washington, Programming Languages, Homework 7, hw7runner.rb

require_relative './hw7provided'
require_relative './hw7assignment'

def runTetris
  Tetris.new 
  mainLoop
end

def runMyTetris
  MyTetris.new
  mainLoop
end

def runChallengeTetris
	MyTetrisChallenge.new
	mainLoop
end

if ARGV.count == 0
  runMyTetris
elsif ARGV.count != 1
  puts "usage: hw7runner.rb [enhanced | original | challenge]"
elsif ARGV[0] == "enhanced"
  runMyTetris
elsif ARGV[0] == "original"
  runTetris
elsif ARGV[0] == "challenge"
  runChallengeTetris
else
  puts "usage: hw7runner.rb [enhanced | original | challenge]"
end

