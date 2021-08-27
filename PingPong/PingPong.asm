;------------------------------------------
;-------------- PING PONG -----------------
;------------------------------------------
;
;
; This game functions similarly to classic pong.
; When the game starts, the player must select 1-player or 2-player.
; In 1-player mode, the player controls the left paddle, and the CPU controls the right paddle.
; In 2-player mode, player 1 controls the left paddle, and player 2 controls the right paddle.
; After the mode is selected, the game field appears and the ball spawns in the center.
; After a brief moment, the ball starts moving randomly to the right or left.
; When the ball hits the top or bottom wall, it bounces and continues moving.
; When the ball his a paddle, it reflects with added direction based on where on the paddle it hits.
; There are several regions on the paddle that effect how it reflects, which will make for more interesting play.
; When the ball moves passed a paddle, the player opposite scores a point.
; Points are displayed at the top of the screen. 
; When a player reaches 7 points, the winning player is displayed, and the start screen is displayed again.
;
; Game States: Start -> OnePlayer || TwoPlayer -> GameOver ]
;                ^------------<-------------<-------------<|
;
; DEVELOPMENT STAGES
; Stage 1:
; Design playfield, paddle, and ball.
; Implement ball movement and manual paddle control.
; 
; Stage 2:
; Implement score. 
; Implement GameOver state, to display winner and pause before restarting.
;
; Stage 3:
; Design start screen
; Select mode, transition to play state
; Implement move from GameOver state to Start state.
; 
; Stage 4:
; Implement CPU in OnePlayer state.
; Implement player 2 controls in TwoPlayer state.
; Transition between all states.
;
; Stage 5:
; Add sound effects for ball hitting paddle and wall.
; Add game start music when ball has spawned, before ball starts moving.
; Add a tune for when a player scores.
; Add looping music for Start state.
;
; Stage 6:
; Touch up visuals. Nicer background and sprites. Change colors. Get creative.
;

;Test2