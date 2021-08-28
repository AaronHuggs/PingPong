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
        ;Paddles
            PaddleSpeed = 2 ;Speed of the paddles
        ;Wall Boundaries
            RIGHTWALL = $E9 ;Define boundaries
            TOPWALL = $08
            BOTTOMWALL = $DE
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
            BallSpeed: .res 1
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
        jsr InitializeStats
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
        jsr ReadControllers
        rti
;Game Functions
    InitializeStats:
        ;Set Ball Speed
            lda #01
            sta BallSpeed
        rts
    DisplaySprites:
        lda #$00
        sta OAMADDR ;Set low byte (00) of sprite RAM address
        lda #$02
        sta OAMDMA ;Set high byte (02) of sprite RAM address and begin DMA transfer
        rts

    ReadControllers: ;---------------------------------------
        lda #$01
        sta $4016
        lda #$00
        sta $4016 ; tell both the controllers to latch buttons

        ldx #$08 ; loop 8 times for all 8 buttons
        ReadControllerLoop:
            lda $4016 ;get current buttons
            lsr a ;move bit 0 to carry
            rol buttons ;move carry into buttons register
            dex ;decrease x
            bne ReadControllerLoop
        ;A,B,Select,Start,Up,Down,Left,Right
        ReadStart:
            lda buttons
            and #%00010000 ;bit 5 = start
            beq ReadStartDone ;branch if button is not pressed

            ReadStartDone:
        ReadUp:
            lda buttons
            and #%00001000 ;bit 4 = up
            beq ReadUpDone ;branch if button is not pressed
            
            ;Get Paddle Sprite info
            lda #$00
            sta SpritePointer
            lda #$02
            sta SpritePointer+1
            MoveUp:
                ldx #$00
                ldy #$04 ;Address of first sprite Y pos, $0204
                MoveUpLoop:
                    lda (SpritePointer), y; Get Y pos of current sprite
                    sec
                    sbc #PaddleSpeed ;y - speed
                    tax ;save the new position
                    sec
                    sbc #TOPWALL ;check collision with top wall
                    bcc ReadUpDone ;if there is a collision, don't move up
                    txa ;if there isn't, load the new position
                    sta (SpritePointer), y ;move up
                    tya 
                    clc
                    adc #$04 ;get next sprite y pos
                    tay
                    cmp #$18 ;3 Paddle sprites, 4 bytes each, addresses $0204-0217.
                    bcc MoveUpLoop

            ReadUpDone:
        ReadDown:
            lda buttons
            and #%00000100 ;bit 3 = down
            beq ReadDownDone ;branch if button is not pressed

            ;Get Paddle Sprite info
            lda #$00
            sta SpritePointer
            lda #$02
            sta SpritePointer+1
            MoveDown:
                ldx #$00
                ldy #$14 ;Bottom paddle sprite y pos = $0214
                MoveDownLoop:
                    lda (SpritePointer), y
                    clc
                    adc #PaddleSpeed
                    tax
                    lda #BOTTOMWALL
                    sbc (SpritePointer), y
                    bcc ReadDownDone
                    txa
                    sta (SpritePointer), y
                    tya
                    sec
                    sbc #$04
                    tay
                    cmp #$04
                    bcs MoveDownLoop

            ReadDownDone:
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
            LoadSpritesLoop:
                lda BallSprite, X
                sta $0200, x
                inx
                cpx #$2C ;See Sprites
                bne LoadSpritesLoop
            rts

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
    ;Sprites
        BallSprite:
            .byte $78,$00,$00,$78 ;0-3
        PaddleOneSprite:
            .byte $70,$01,$00,$18 ;4-7
            .byte $78,$02,$00,$18 ;8-B
            .byte $80,$02,$00,$18 ;C-F
            .byte $88,$02,$00,$18 ;10-13
            .byte $90,$03,$00,$18 ;14-17
        PaddleTwoSprite:
            .byte $70,$01,$00,$E0 ;18-1B
            .byte $78,$02,$00,$E0 ;1C-1F
            .byte $80,$02,$00,$E0 ;20-23
            .byte $88,$02,$00,$E0 ;24-27
            .byte $90,$03,$00,$E0 ;28-2B
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