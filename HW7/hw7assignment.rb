# Klemen Kotar, CSE 341 AA, Homework 7

class MyPiece < Piece
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               rotations([[0, 0], [-1, 0], [-1, -1], [0, -1], [1, -1]]), # Custom Piece 1
               [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # Custom piece 2
           	   [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
           	   rotations([[0, 0], [0, 1], [1, 1]])] # Custom piece 3

  # class method to choose the next piece
  # this methode is overwriten so that new pieces are chosen from the class array 
  # All_pieces inside MyBoard (instead of Board) 
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # this methode is overwriten to set @current_block to an instance of MyPiece (instead of Piece)
  def initialize (game)
  	super
    @current_block = MyPiece.next_piece(self)
    @cheat = false # the code would run without this, since Nil is treated as false, but that would be bad style
  end

  # rotates the current piece by 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # cheat sets @cheat boolean to true and reduces score by 100 if conditions are met
  def cheat
  	if !game_over? and @game.is_running? and score >= 100 and !@cheat
  		@score = @score -100
  		@game.update_score
  		@cheat = true
  	end
  end

  # gets the next piece, modified to include the cheat case
  def next_piece
  	if @cheat # if cheat was selected, make the next piece be a sinlge block
  		@current_block = MyPiece.new([[[0, 0]]], self)
  		@cheat = false
  	else
    	@current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index| #iterate over as many elements as ther are blocks in the piece
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  # this methode is overwriten to set @board to an instance of MyBoard (instead of Board)
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
  	super
    # bind the 'u' key to board's rotate_180 methode
	@root.bind('u', lambda {@board.rotate_180})
	# bind the 'c' key to the board's cheat methode
	@root.bind('c', lambda {@board.cheat})
  end
end




# TETRIS CHALLENGE

class MyPieceChallenge < MyPiece
	def initialize(point_array, board)
		super
		@base_position = [10, 0] # move base position to center of screen
	end

	#overwrite next_piece to generate instances of MyPieceChallenge
	def self.next_piece (board)
    	MyPieceChallenge.new(All_My_Pieces.sample, board)
  	end

	# acess to the color filed is given for custom block colors
	attr_accessor(:color)
end

class MyBoardChallenge < MyBoard

  def initialize (game)
  	super
    @current_block = MyPieceChallenge.next_piece(self)
    @cheat = false # the code would run without this, since Nil is treated as false, but that would be bad style
    @bomb = false
    @armed = false
  end

  # block size is increased
  def block_size
    20
  end
  
  # game size is increased
  def num_columns
    20
  end

  def num_rows
    40
  end

  # cheat sets @cheat boolean to true and reduces score by 100 if conditions are met
  def cheat
  	if !game_over? and @game.is_running? and score >= 100 and !@cheat and !@bomb
  		@score = @score -100
  		@game.update_score
  		@cheat = true
  		@game.put_message('CHEAT')
  	end
  end

  # bomb sets @bomb boolean to true and reduces score by 500 if conditions are met
  def bomb
  	if !game_over? and @game.is_running? and score >= 500 and !@cheat and !@bomb
  		@score = @score -500
  		@game.update_score
  		@bomb = true
  		@game.put_message('BOMB') # display bomb
  	end
  end

  # gets the next piece, modified to include the cheat case
  def next_piece
  	if @cheat # if cheat was selected, make the next piece be a sinlge block
  		@current_block = MyPieceChallenge.new([[[0, 0]]], self)
  		@cheat = false
  	elsif @bomb
  		@current_block = MyPieceChallenge.new([[[0, 0]]], self)
  		@current_block.color = 'black'
  		@bomb = false
  		@armed = true
	else
    	@current_block = MyPieceChallenge.next_piece(self)
    end
    @current_pos = nil
  end

  # Method rewritten to account for 'bomb' case which blows up a 5X5 square around the bomb block
  def store_current
  	displacement = @current_block.position
  	locations = @current_block.current_rotation
	    (0..(locations.size-1)).each{|index| #iterate over as many elements as ther are blocks in the piece
	      current = locations[index];
	      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
	      @current_pos[index]
	    }
  	if @armed # if bomb is dropped remove the blocks in a 5X5 radius around the block
	    (-2..2).each do |x|
			(-2..2).each do |y|
				posX = x + displacement[0]
				posY = y + displacement[1]
				if posX > 0 and posY > 0 and @grid[posY][posX] != nil
					@grid[posY][posX].remove;
					@grid[posY][posX] = nil;
				end
			end
		end
		@armed = false
		@game.put_message('BOOM!')
	end
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # drops the piece to the lowest location in the currently occupied columns.
  # Then replaces it with a new piece
  # Change the score to reflect the distance dropped.
  # Displays a message alerting how many points were scored
  def drop_all_the_way
    if @game.is_running?
      ran = @current_block.drop_by_one
      @current_pos.each{|block| block.remove}
      acc = 0 # init accumulator to count how many points were scored
      while ran
      	acc += 1
        ran = @current_block.drop_by_one
      end
      @score += acc
      draw
      @game.put_message('+' + acc.to_s)
      store_current
      if !game_over?
        next_piece
      end
      @game.update_score
      draw
    end
  end

  # removes all filled rows and replaces them with empty ones, dropping all rows
  # above them down each time a row is removed and increasing the score.  
  def remove_filled
    (2..(@grid.size-1)).each{|num| row = @grid.slice(num);
      # see if this row is full (has no nil)
      if @grid[num].all?
        # remove from canvas blocks in full row
        (0..(num_columns-1)).each{|index|
          @grid[num][index].remove;
          @grid[num][index] = nil
        }
        # move down all rows above and move their blocks on the canvas
        ((@grid.size - num + 1)..(@grid.size)).each{|num2|
          @grid[@grid.size - num2].each{|rect| rect && rect.move(0, block_size)};
          @grid[@grid.size-num2+1] = Array.new(@grid[@grid.size - num2])
        }
        # insert new blank row at top
        @grid[0] = Array.new(num_columns);
        # adjust score for full flow - bump this to 100
        @score += 100;
        @game.put_message('+100') # display score

      end}
    self
  end

end


class MyTetrisChallenge < MyTetris

  # this methode is overwriten to set @board to an instance of MyTetrisRootChallenge (instead of TetrisRoot)
  def initialize
    @root = MyTetrisRootChallengre.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    buttons
    run_game
  end

  # creates a canvas and the board that interacts with it
  # this methode is overwriten to set @board to an instance of MyChallengeBoard (instead of MyBoard)
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # a binding for 'b' is added which triggers the bomb cheat
  def key_bindings  
  	super
    # bind the 'b' key to board's bomb methode
	@root.bind('b', lambda {@board.bomb})
  end

  # new button methode to move the buttons on larger game window + add message box
  def buttons
    pause = TetrisButton.new('pause', 'lightcoral'){self.pause}
    pause.place(35, 50, 90, 7)

    new_game = TetrisButton.new('new game', 'lightcoral'){self.new_game}
    new_game.place(35, 75, 15, 7)
    
    quit = TetrisButton.new('quit', 'lightcoral'){exitProgram}
    quit.place(35, 50, 140, 7)
    
    move_left = TetrisButton.new('left', 'lightgreen'){@board.move_left}
    move_left.place(35, 50, 527, 736)
    
    move_right = TetrisButton.new('right', 'lightgreen'){@board.move_right}
    move_right.place(35, 50, 627, 736)
    
    rotate_clock = TetrisButton.new('^_)', 'lightgreen'){@board.rotate_clockwise}
    rotate_clock.place(35, 50, 577, 701)

    rotate_counter = TetrisButton.new('(_^', 'lightgreen'){
      @board.rotate_counter_clockwise}
    rotate_counter.place(35, 50, 577, 771)
    
    drop = TetrisButton.new('drop', 'lightgreen'){@board.drop_all_the_way}
    drop.place(35, 50, 577, 736)

    label = TetrisLabel.new(@root) do
      text 'Current Score: '   
      background 'lightblue'
      font 'helvetica 36 bold'
    end
    label.place(35, 300, 226, 15)
    @score = TetrisLabel.new(@root) do
      background 'lightblue'
      font 'helvetica 36 bold'
    end
    @score.text(@board.score)
    @score.place(35, 80, 500, 15)

    @message = TetrisLabel.new(@root) do
      text '+42'   
      background 'red'
      font 'helvetica 36 bold'
      foreground 'white'
    end

    @cover = TetrisLabel.new(@root) do  
      background 'lightblue'
    end   

  end

  # methode the displays a message and makes it dissapear after 400ms
  def put_message text
  	buttons
  	@message.text(text)
  	@message.place(50,150, 500, 450)

  	message_timer = TetrisTimer.new
  	message_timer.start(400, (lambda { |x| @cover.place(50,150, 500, 450)}))
  end

end

# new Rooot class that changes dimensions of game window
class MyTetrisRootChallengre < TetrisRoot
  def initialize
    @root = TkRoot.new('height' => 900, 'width' => 750, 
             'background' => 'lightblue') {title "Tetris"}    
  end

  # Necessary so we can unwrap before passing to Tk in some instances.
  # Student code MUST NOT CALL THIS.
  attr_reader :root

end