#Klemen Kotar, CSE 341 HW7 Testing File

require 'minitest/autorun'
load 'hw7provided.rb'
load 'hw7assignment.rb' 


# subclass board to add acessors
class BoardTest < MyBoard
	attr_accessor(:score, :current_block, :All_Pieces)
end

# subclass piece to add acessors
class PieceTest < MyPiece
	attr_accessor(:rotation_index)
end


class TestHw7 < Minitest::Test

	def setup
		@game = MyTetris.new
		@board = BoardTest.new(@game)
		@piece = MyPiece.next_piece(@board)
	end

	def test_flip
		# test if piece is flipped across 180 properly
		@board.current_block = PieceTest.new([[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], 
           	   [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]], @board)
		@board.current_block.rotation_index = 0 # set index to 0
		@board.rotate_180
		assert_equal(@board.current_block.current_rotation, [[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]])

		# test if more complex piece is flipped across 180 properly
		@board.current_block = PieceTest.new(Piece.rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), @board)
		@board.current_block.rotation_index = 0 # set index to 0
		@board.rotate_180
		assert_equal(@board.current_block.current_rotation, [[0, 0], [-1, 0], [0, 1], [1, 1]])

		# test if a single piece is flipped across 180 properly (it stays the same)
		@board.current_block = PieceTest.new([[[0, 0]]], @board)
		@board.current_block.rotation_index = 0 # set index to 0
		@board.rotate_180
		assert_equal(@board.current_block.current_rotation, [[0, 0]])

		# test if the custom piece is flipped across 180 properly (it stays the same)
		@board.current_block = PieceTest.new(Piece.rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, -1]]), @board)
		@board.current_block.rotation_index = 0 # set index to 0
		@board.rotate_180
		assert_equal(@board.current_block.current_rotation, [[0, 0], [1, 0], [1, 1], [0, 1], [-1, 1]])
	end

	# tests dealing with the functionality of the cheat methode
	def test_cheat_button
		# check if cheat works when score is 0
		@board.cheat
		assert_equal(0, @board.score)

		# check if the score rises to 200 after setter is called
		@board.score = 200
		assert_equal(200, @board.score)

		# check if the score drops to 100 after cheat is called
		@board.cheat
		assert_equal(100, @board.score)

		# check if the score stays at 100 after the second cheat is called
		@board.cheat
		assert_equal(100, @board.score)
	end

	# test dealing with the generation of the cheat piece
	def test_cheat_piece
		# set score to 200, call cheat and fetch next piece to see if it is the cheat piece
		@board.score = 200
		@board.cheat
		@board.next_piece
		assert_equal(@board.current_block.current_rotation, [[0, 0]])

		# test to see if starting a new game with the cheat piece on the board geenrates wierd behaviour
		@game.new_game
		@board.next_piece
		refute_equal(@board.current_block.current_rotation, [[0, 0]])
	end

	# test if hitting c and n one after the other causes bugs
	def test_new_game
		@board.score = 200
		@board.cheat
		@board.run
		@game.new_game
		@board.run
		refute_equal(@board.current_block.current_rotation, [[0, 0]])
	end


end  