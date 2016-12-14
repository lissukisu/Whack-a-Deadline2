package kierros1
import processing.core._
import scala.concurrent.duration._
import scala.util
import helper.TicTacToeHelper._

object TicTacToe extends PApplet  {
  

  
  val boxSize: Int = 120
  val boxAmount: Int = 3
  val gameSize: Int = boxAmount * boxSize
  private var canStartNewGameTime: Option[Deadline] = None // älkää välittäkö, liittyy aikaan
  
  val zero = loadImage("O.png")  
  val cross = loadImage("X.png")
  var back = loadImage("Grass.jpg") //taustakuva
  back.resize(gameSize,gameSize) //taustakuva oikeaan kokoon
  var button = loadImage("start.png") // aoitusnappula XD
  button.resize(100,60)
  var deadline = loadImage("Deadline.jpg") // deadline-kuva
  
  
  val oPlayer: String = "O"
  val xPlayer: String = "X" 
  
  
  private var turn = deadline
  
  private var isGameOn: Boolean = false
  
  private var begin: Boolean = true
  
  private var grid = Array.fill(boxAmount, boxAmount)("")
  
  private var clickAmounts = 0
  
  
  /**
   * The initialization method for the game.
   */
  
  override def setup() : Unit = { 
    size(gameSize, gameSize) 
  }
 
  

  
  /** 
   *  The method draw is used to draw the background and the grid to the tic tac toe -game, if the game is on.
   *  If the game has ended, the method only draws the background and a textual description of how the game ended.
   */

  override def draw() : Unit = {
    if (isGameOn) {
      image(back,0,0) // taustakuvan asettaminen
      stroke(0)
      drawGrid(this, boxAmount, boxSize)
      redrawImages(grid, deadline, oPlayer, deadline, xPlayer, boxSize, this)
    } else if (begin) { // jäätävä aloitusnäkymä
      image(back,0,0)
      text("Welcome to play Whack-a-Deadline!",50,50)
      image(button,130,150)
    } else {
        image(back,0,0)
        noLoop()
        textSize(30)
        fill(90)
        text("Game over!",30,50)
         if  (!didGameEnd(grid, oPlayer) && !didGameEnd(grid, xPlayer))  text("It's a tie!", 30, 100)
         else if (getTurn == "O")  text ("X won :-)", 30, 100)
         else if (getTurn == "X")  text("O won :-)", 30, 100)
      }
  }

  
  
  /**
   * When mouse is clicked the location is counted and if the place on the board is empty player's sign is placed there.
   * If one player wins or the game ends in a tie (no empty boxes are left), the game can't be continued. 
   */
         
  override def mouseClicked() : Unit = {
    var xCordinate: Int = mouseX / boxSize
    var yCordinate: Int = mouseY / boxSize
    if (isGameOn && grid(xCordinate)(yCordinate) == "") {
      clickAmounts += 1
      image(turn, xCordinate * boxSize, yCordinate * boxSize)
      grid(xCordinate)(yCordinate) = getTurn 
        if (didGameEnd(grid, getTurn) || clickAmounts == boxAmount * boxAmount) {
        isGameOn = false    
        }
      this.changeTurn()  
    }
  }
 

  
  override def keyPressed() { 
    if (key == ' ') isGameOn = true // spacella peli alkaa
    else if (isGameOn == true) { // Kuva katoaa näppäimellä, jos se kuva on laitettu siihen klikkaamalla (mouseclicked)
      if (key == 'q' | key == 'Q') {
        grid(0)(0) = ""
      }
      redrawImages(grid, deadline, oPlayer, deadline, xPlayer, boxSize, this)
    } 
  }
  
  

  /**
   * Returns the turn as a String.
   */
  
  private def getTurn() : String = { 
    if (turn == zero)
      "O"
    else "X" 
  }

  

  /**
   * Changes turn from O to X, and vice versa.
   */
  private def changeTurn() : Unit = { 
    if (turn == zero)
      turn = cross
    else
      turn = zero  
  }
  
  def playGame() {
    
  }
  
  
    /**
   * The main method that makes your application run and show.
   */
  def main(args: Array[String]) {
    val frame = new javax.swing.JFrame("TicTacToe")

    frame.getContentPane().add(this)
    init
    frame.setSize(this.getSize())
    frame.pack
    frame.setVisible(true)
    frame.setLocationRelativeTo(null)
    frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)
  }
  
  
  
  
}
