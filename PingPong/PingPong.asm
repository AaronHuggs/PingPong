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
;------------------------------------------

.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02
    .byte $01
    .byte %00000000
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00,$00,$00,$00,$00
.segment "STARTUP"
.segment "ZEROPAGE"
;----VARIABLES----
    ;Constants
        ;States
            StateStart = 0 ;display title screen
            StateOnePlayer = 1 ;one-player mode
            StateTwoPlayer = 2 ;two-player mode
            StateGameOver = 3 ;display game over screen
        ;Ball
            BallSpeed = 1 ;Speed of the ball
        ;Wall Boundaries
            RIGHTWALL = $E9 ;Define boundaries
            TOPWALL = $0F
            BOTTOMWALL = $D9
            LEFTWALL = $10
        ;NES Registers
            PPUCTRL = $2000
            PPUMASK = $2001
            PPUSTATUS = $2002
            OAMADDR = $2003
            PPUSCROLL = $2005
            PPUADDR = $2006
            PPUDATA = $2007
            OAMDMA = $4014
            APUSTATUS = $4015
            SQ1_ENV = $4000
            SQ1_LO = $4002
            SQ1_HIGH = $4003
        
    ;Variables
        gamestate: .res 1 ;stores current game state
        buttons: .res 1 ;used for checking buttons pressed
        ;Ball
            ballx: .res 1 ;ball horizontal position
            bally: .res 1 ;ball vertical position
            ballup: .res 1 ;1 = ball moving up
            balldown: .res 1 ;1 = ball moving down
            ballleft: .res 1 ;1 = ball moving left
            ballright: .res 1 ;1 = ball moving right
            ballspeedx: .res 1 ;ball horizontal speed per frame
            ballspeedy: .res 1 ; ball vertical speed per frame
        ;Score
            score: .res 1 ; score only goes up to 7, so one byte is enough

.segment "CODE"
;NES Initialization
    vblankwait:
        BIT PPUSTATUS
        BPL vblankwait
        rts
    RESET:
        SEI          ; disable IRQs
        CLD          ; disable decimal mode
        LDX #$40	
        STX $4017    ; disable APU frame IRQ
        LDX #$FF	
        TXS          ; Set up stack
        INX          ; now X = 0
        STX PPUCTRL    ; disable NMI
        STX PPUMASK    ; disable rendering
        STX $4010    ; disable DMC IRQs

        jsr vblankwait

    clrmem:
        LDA #$00
        STA $0000, x
        STA $0100, x
        STA $0200, x
        STA $0400, x
        STA $0500, x
        STA $0600, x
        STA $0700, x
        LDA #$FE
        STA $0300, x
        INX
        BNE clrmem
        
        jsr vblankwait
;Setup
    setupFunctions:

;Wait for vblank interrupt
    Forever:
        jmp Forever

;Vblank 
    VBLANK: ;Runs every frame

;Graphics
    ;Palettes
        background_palette:
            .byte $22,$27,$17,$0F	;background palette 1 starting at $3F00
            .byte $22,$14,$17,$0F	;background palette 2
            .byte $22,$15,$21,$0F	;background palette 3
            .byte $22,$27,$17,$0F	;background palette 4
            
        sprite_palette:
            .byte $22,$27,$35,$0F	;sprite palette 1 starting at $3F10
            .byte $22,$27,$17,$0F	;sprite palette 2
            .byte $22,$1C,$15,$14	;sprite palette 3
            .byte $22,$02,$38,$3C	;sprite palette 4

    ;Sprites

    ;Nametables
        ;Background
        
        ;Attribute

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
 