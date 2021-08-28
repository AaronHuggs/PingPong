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
        BGPointer: .res 2 ;used to select address when loading background
        SpritePointer: .res 2 ;used to select address when loading sprites
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
        jsr SetupPalettes
        jsr LoadPlayStateBG
        jsr LoadSprites
        jsr LoadAttribute
        FinishSetup:
            LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
            STA PPUCTRL
            LDA #%00011010   ; enable sprites(5), enable background(4), enable background on left side(2)
            STA PPUMASK
            LDA #$00        ;tell the ppu there is no background scrolling
            STA PPUSCROLL
            STA PPUSCROLL

;Wait for vblank interrupt
    Forever:
        jmp Forever

;Vblank 
    VBLANK: ;Runs every frame
        jsr DisplaySprites

        rti
;Game Functions
    DisplaySprites:
        lda #$00
        sta OAMADDR ;Set low byte (00) of sprite RAM address
        lda #$02
        sta OAMDMA ;Set high byte (02) of sprite RAM address and begin DMA transfer
        rts

;Graphics
    ;Setup Palettes
        SetupPalettes: 
            lda PPUSTATUS ;Read PPU status to reset high/low byte latch to high
            lda #$3F
            sta PPUADDR ; store high byte of $3F00 address
            lda #$00
            sta PPUADDR ; store low byte of $3F00 address

            ;loop through the palette data to store it all into the ppu
            ldx #$00 ;start at 0
            LoadBackgroundPaletteLoop:
                lda background_palette, x ; load from address PaletteData+x 
                sta PPUDATA ; ppu data register
                inx ;increment x
                cpx #$10 ;compare x to decimal 16
                bne LoadBackgroundPaletteLoop

            ldx #$00
            LoadSpritePaletteLoop:
                lda sprite_palette, X
                sta PPUDATA
                inx
                cpx #$10
                bne LoadSpritePaletteLoop
            rts

        ;Palettes
            background_palette:
                .byte $00,$0F,$10,$30	;background palette 1 starting at $3F00
                .byte $00,$0F,$10,$30	;background palette 2
                .byte $00,$0F,$10,$30	;background palette 3
                .byte $00,$0F,$10,$30	;background palette 4
                
            sprite_palette:
                .byte $00,$0F,$10,$30	;sprite palette 1 starting at $3F10
                .byte $00,$0F,$10,$30	;sprite palette 2
                .byte $00,$0F,$10,$30	;sprite palette 3
                .byte $00,$0F,$10,$30	;sprite palette 4

    ;Setup Backgrounds
        LoadPlayStateBG:
            lda PPUSTATUS ;read PPU status to reset the high/low latch
            lda #$24
            sta PPUADDR ;write the high byte of $2000 address
            lda #$00
            sta PPUADDR ;write the low byte of $2000 address

            lda #<PlayStateBG
            sta BGPointer
            lda #>PlayStateBG
            sta BGPointer+1
            
            ldx #$00 ;start out at 0
            ldy #$00
            LoadPlayStateBGLoop:
                lda (BGPointer),y
                sta PPUDATA
                iny
                cpx #$03
                bne :+
                cpy #$C0
                beq DoneLoadingPlayStateBG    
            :
                cpy #$00
                bne LoadPlayStateBGLoop
                inx
                inc BGPointer+1
                jmp LoadPlayStateBGLoop
            DoneLoadingPlayStateBG:
                rts
    ;Setup Sprites
        LoadSprites: 
            ldx #$00
            LoadBallSprite:
                lda BallSprite, x
                sta $0200, x
                inx
                cpx #$04 
                bne LoadBallSprite
            ldx #$00
            LoadPaddleOneSprite:
                lda PaddleOneSprite, x
                sta $0204, x
                inx
                cpx #$0C ; 3 sprites, 4 x 3 = 12 ($0C)
                bne LoadPaddleOneSprite
            ; Sprites are ready!
            rts
        BallSprite:
            .byte $80,$00,$00,$40
        PaddleOneSprite:
            .byte $60,$01,$00,$10
            .byte $68,$02,$00,$10
            .byte $70,$03,$00,$10

    ;Setup Attribute
        LoadAttribute:
            lda PPUSTATUS
            lda #$23
            sta PPUADDR ;write the high byte of $23C0 address
            lda #$C0
            sta PPUADDR ;write the low byte of $23C0 address

            ldx #$00
            LoadAttributeLoop:
                lda attribute,x
                sta PPUDATA
                inx 
                cpx #$40
                bne LoadAttributeLoop

            rts

    ;Nametables
        ;Background
            PlayStateBG:
                .incbin "PingPongPlayState.nam"
        ;Attribute
            attribute:
                .incbin "attributes.nam"

.segment "VECTORS"
    .word VBLANK
    .word RESET
    .word 0

.segment "CHARS"
    .incbin "PingPongChar.chr"