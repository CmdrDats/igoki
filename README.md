# igoki

Bridge the gap between playing Go on a physical board and digitally.

The aim of igoki is to enable Go players to play on a physical board, but still have the benefits
of playing via a digital medium:

 - Record your Kifu (game record) automatically as the game progresses
 - Guided reviewing, so that you can review games and explore branches on your physical board easily
 - Direct online playing using integration with online-go.com

It is preferable to setup an external webcam to have a decently lit top-down view of your Go board,
but it can handle a laptop webcam as long as you can fit the whole board into the picture. Sharper
angles do make the stone detection difficult due to stones occluding each other, so try raise your
laptop as high as you can.

When you start, you will be asked to select the 4 points of your board - you can flip between camera
inputs, in case the first selected one is not the one you want to use. Once you have selected
and adjusted that, you will be shown a differenced view where you can shift the sampling points
to take the stone height into consideration.

From there, the Game mode wil allow you to record and export your game into an SGF file.

There is still much to do in the way of features, but the major feature shortlist currently are
 (in oder of priority) :

 - Decent reviewing support: Load SGF, step back and forth through the game, create branches and 
   explore game variations. 
   
 - Online-go integration: Upload game record automatically, play live or correspondence games, watch
   current live games, review games. Play a physical ranked game with two players both providing
   usernames and api keys and then submit the moves on their behalf (allowing to continue
   correspondence games later)
   
 - Feedback onto the board: For the 'next move' on a review or live game, provide some way of
   showing where the coordinate is. First prize is to use a projector to actually project the stone
   onto the board. The route I will currently pursue is to setup an Arduino with 38 LED's
   (19 LED's for a-s and 19 for 1-19), perhaps two more for B/W indication with a serial USB link
   to update the coordinates.
   
## Usage

Clone this repo and run `lein run`, it will start up the frame and guide you through calibration.

Alternatively, if you are doing development on this project, fire up a `lein repl` and `(start)` 
to get started.

## Contributions

This project is still in it's early stages and I'm excited about the possibilities, but I would
dearly appreciate code contributions to the project. If you find this useful but you can't contribute,
 please consider supporting this project by donating to me on paypal : cmdrdats@gmail.com. Thanks! :)
 
## License

Copyright © 2015 Deon Moolman

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
