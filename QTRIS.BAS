'Qtris Copyright (C) 1994, Michael Teator
'
'This program is free software; you can redistribute it and/or
'modify it under the terms of the GNU General Public License
'as published by the Free Software Foundation; either version 2
'of the License, or (at your option) any later version.
'
'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.
'
'You should have received a copy of the GNU General Public License
'along with this program; if not, write to the Free Software
'Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

DEFINT A-Z                      'Set default data to integer for speed

TYPE Score                      'Type to keep a players score
    Lines AS INTEGER
    Points AS INTEGER
    Level AS INTEGER
END TYPE

TYPE Tetris                     'Type to keep a tetris piece's info
    Piece AS INTEGER
    Row AS INTEGER
    Col AS INTEGER
    Sky AS INTEGER
END TYPE

TYPE VGA                        'Type to hold a color's value
    Red AS INTEGER
    Green AS INTEGER
    Blue AS INTEGER
END TYPE

TYPE Highs                      'Type to hold high score info
    Player AS STRING * 20
    Score AS INTEGER
END TYPE

DECLARE FUNCTION DrawPiece (Current AS Tetris, Old AS Tetris, OnlyErase)
DECLARE FUNCTION GetName$ ()
DECLARE FUNCTION Menu ()
DECLARE FUNCTION ScoreSystem (NewLines, Level)
DECLARE FUNCTION Search (Highest() AS Highs, Goal)
DECLARE FUNCTION TestPiece (Piece AS Tetris)
DECLARE FUNCTION UpDateMatrix ()
DECLARE SUB Delay (HundSec AS SINGLE)
DECLARE SUB DispPic (Num)
DECLARE SUB DrawBox (Col, Row, Clr, Condition, BoxType)
DECLARE SUB DrawFrame (tx, ty, bx, by)
DECLARE SUB DrawNextPiece (Shape)
DECLARE SUB DrawScreen (Level, TypeDraw)
DECLARE SUB EndScreen ()
DECLARE SUB ErrorHandler (ErrLevel)
DECLARE SUB Fade (TypeFade)
DECLARE SUB Help ()
DECLARE SUB HighScores ()
DECLARE SUB InitColors ()
DECLARE SUB Initialize ()
DECLARE SUB InitPieces ()
DECLARE SUB InitVGA ()
DECLARE SUB Main ()
DECLARE SUB Options ()
DECLARE SUB Pause (CurrentLevel)
DECLARE SUB PlayGame ()
DECLARE SUB PlaySound (WhichSound)
DECLARE SUB PrintScore (Player AS Score)
DECLARE SUB PutRandomBlocks ()
DECLARE SUB RedrawMatrix (RowStart, RowEnd)
DECLARE SUB ResetMatrix ()
DECLARE SUB RotatePalette (PalFile AS STRING)
DECLARE SUB SaveHigh ()
DECLARE SUB ScreenSaver (CurrentLevel)
DECLARE SUB SetColor (Index, NewClr AS VGA)
DECLARE SUB Sort (Highest() AS Highs)
DECLARE SUB TitleScreen ()
DECLARE SUB WaitForKey (HundSec AS SINGLE)

CONST BoxX = 11, BoxY = 9
CONST PopIn = 1, PopOut = 2, FadeIn = 3, FadeOut = 4
CONST False = 0, True = -1
CONST DrawP = True, EraseP = False
CONST SkyUp = 1, SkyRight = 2, SkyDown = 3, SkyLeft = 4
CONST Square = 1, RightLeg = 2, LeftLeg = 3, Straight = 4, RightStep = 5, LeftStep = 6, Peg = 7
CONST LeftMargin = 92, TopMargin = 5
CONST NoBlock = 0, WallPiece = -1, DeadBlock = -2
CONST ScoreString = "######"
CONST Rotate = 1, HitBottom = 2, HitWall = 3, GotTetris = 4, GameOver = 5, Swapped = 6
CONST ResetScore = -1, FinalScore = -2
CONST OnlyErase = True, All = False
CONST GreyValue = 22
CONST Redraw = False, New = True
CONST ScreenSaverPal = "save.pal"
CONST ConfigFile = "qtris.ini"
CONST ScoreFile = "qtris.scr"

OPTION BASE 1

DIM SHARED TetrisPieces(7, 4, 4, 4)     'Important globals that the whole
DIM SHARED PieceClr(10)                 'program has to access
DIM SHARED Cols(0 TO 20), Rows(21)
DIM SHARED Pal(0 TO 255, 3)
DIM SHARED ScreenMatrix(-4 TO 23, 0 TO 24)
DIM SHARED StartLines, StartLevel, GameSound, Fading
DIM SHARED Byte AS STRING * 1
DIM SHARED FileName(39) AS STRING
DIM SHARED Grey AS VGA, Black AS VGA

ON ERROR GOTO ErrorTrapping             'Trap anny errors

Main                                    'Start the show

ErrorTrapping:
    ErrorHandler (ERR)
   

'----- program data -----
PicFiles:
DATA "Title", "Menu", "End", "Help", "Option", "Save", "High", "Name"

SUB Delay (HundSec AS SINGLE)

    HundSec = HundSec / 100
    Time1! = TIMER
    DO
        Time2! = TIMER - Time1!
    LOOP UNTIL Time2! > HundSec

END SUB

SUB DispPic (Num)
   
    DIM Pic(1 TO 16002) AS INTEGER     'Dimension a array to hold the picture
   
    OPEN FileName(Num) + ".pal" FOR BINARY AS #1  'open the picture's palette
    
    FOR Index = 0 TO 255              'copy the palette to the Pal array
    OUT &H3C8, Index                  'but ignore the first 39 values (used by
        FOR RGB = 1 TO 3              'the game)
            GET #1, , Byte:
            IF Index > 39 THEN Pal(Index, RGB) = ASC(Byte)
            OUT &H3C9, 0              'Set the video card's palette to black so
        NEXT                          'the pic can be faded in later
    NEXT
    CLOSE #1

    DEF SEG = VARSEG(Pic(1))        'set the segment to Pic's address
    FOR y = 1 TO 2
        BLOAD FileName(Num) + "_" + RTRIM$(LTRIM$(STR$(y))) + ".qtr", VARPTR(Pic(1))  'Load the picture into the array
        PUT (0, 0 + (y - 1) * 100), Pic(1), PSET 'Put the Pic on the screen
    NEXT

END SUB

SUB DrawBox (Col, Row, Clr, Condition, BoxType)

    tx = Cols(Col)
    ty = Rows(Row)
    IF (Condition = DrawP) AND (BoxType <> DeadBlock) THEN
        IF BoxType = Moving THEN
            ScreenMatrix(Col, Row) = Clr
        ELSE
            ScreenMatrix(Col, Row) = BoxType
        END IF
        LINE (tx, ty)-(tx + BoxX - 1, ty + BoxY - 1), Clr + 1, B
        LINE (tx + 1, ty + 1)-(tx + BoxX - 1, ty + BoxY - 1), Clr + 2, B
        LINE (tx + 1, ty + 1)-(tx + BoxX - 2, ty + BoxY - 2), Clr, B
        LINE (tx + 2, ty + 2)-(tx + BoxX - 3, ty + BoxY - 3), Clr + 2, B
        LINE (tx + 3, ty + 3)-(tx + BoxX - 3, ty + BoxY - 3), Clr + 1, B
        LINE (tx + 3, ty + 3)-(tx + BoxX - 4, ty + BoxY - 4), Clr, BF
    ELSE
        ScreenMatrix(Col, Row) = BoxType
        LINE (tx, ty)-(tx + BoxX - 1, ty + BoxY - 1), 0, BF
    END IF

END SUB

SUB DrawFrame (tx, ty, bx, by)

    LINE (tx, ty)-(bx, by), 0, BF              'erase underneath the frame
    FOR n = 0 TO 7
        LINE (tx + n, ty + n)-(bx - n, by - n), 39 - n, B   'draw the frame
    NEXT

END SUB

SUB DrawNextPiece (Shape)

    DIM NextPiece AS Tetris
   
    NextPiece.Piece = Shape
    NextPiece.Col = 14
    NextPiece.Row = 4
    NextPiece.Sky = SkyUp
   
    FOR y = 4 TO 7
        FOR x = 14 TO 17
            DrawBox x, y, 0, EraseP, Moving
        NEXT
    NEXT
    Dummy = DrawPiece(NextPiece, NextPiece, All)

END SUB

FUNCTION DrawPiece (Current AS Tetris, Old AS Tetris, EraseOnly)

    DrawPiece = True
    Result = TestPiece(Current)
   
    IF Result THEN
        FOR y = 1 TO 4
            FOR x = 1 TO 4
                IF TetrisPieces(Current.Piece, Old.Sky, x, y) = 1 THEN DrawBox Old.Col + x - 1, Old.Row + y - 1, PieceClr(Current.Piece), EraseP, NoBlock
            NEXT
        NEXT
        IF NOT EraseOnly THEN
            FOR y = 1 TO 4
                FOR x = 1 TO 4
                    IF TetrisPieces(Current.Piece, Current.Sky, x, y) = 1 THEN DrawBox Current.Col + x - 1, Current.Row + y - 1, PieceClr(Current.Piece), DrawP, Moving
                NEXT
            NEXT
        END IF
    ELSE
        DrawPiece = Result
        EXIT FUNCTION
    END IF
    
END FUNCTION

SUB DrawScreen (Level, TypeDraw)

    DispPic 8 + Level
    IF TypeDraw = New THEN
        DrawFrame 100, 80, 220, 120
        LOCATE 13, 17: PRINT "Level"; Level;
        Fade PopIn
        Delay 200
    ELSE
        Fade PopIn
    END IF

    LINE (Cols(1), Rows(1))-(Cols(12) - 1, Rows(21) - 1), 0, BF
    LINE (Cols(13), Rows(3))-(Cols(18) - 1, Rows(20) - 1), 0, BF
    FOR y = 1 TO 21
        IF y < 21 THEN
            DrawBox 0, y, PieceClr(9), DrawP, WallPiece
            DrawBox 12, y, PieceClr(9), DrawP, WallPiece
        ELSE
            FOR x = 0 TO 12
                DrawBox x, y, PieceClr(9), DrawP, WallPiece
            NEXT
        END IF
    NEXT
    FOR y = 2 TO 20
        IF y > 2 AND y < 20 THEN
            DrawBox 18, y, PieceClr(9), DrawP, WallPiece
        ELSE
            FOR x = 13 TO 18
                DrawBox x, y, PieceClr(9), DrawP, WallPiece
            NEXT
        END IF
    NEXT
    COLOR 1: LOCATE 12, 31: PRINT "Lines "
    COLOR 4: LOCATE 15, 31: PRINT "Level "
    COLOR 7: LOCATE 18, 31: PRINT "Points"

END SUB

SUB EndScreen
   
    DispPic 3
    Fade PopIn
    WaitForKey 500
    Fade FadeOut

END SUB

SUB ErrorHandler (ErrorLevel)

    SCREEN 0
    WIDTH 80

    SELECT CASE ErrorLevel
        CASE ELSE
            LOCATE 1, 1
            PRINT "Error Num: "; ErrorLevel
            WaitForKey 100
            SYSTEM
    END SELECT

END SUB

SUB Fade (TypeFade)

    DIM NewPal(0 TO 255, 3)
    
    SELECT CASE TypeFade
        CASE PopIn
            FOR Index = 0 TO 255
                OUT &H3C8, Index
                FOR RGB = 1 TO 3
                    OUT &H3C9, Pal(Index, RGB)
                NEXT
            NEXT
        CASE PopOut
            FOR Index = 0 TO 255
                OUT &H3C8, Index
                FOR RGB = 1 TO 3
                    OUT &H3C9, 0
                NEXT
            NEXT
        CASE FadeIn
            FOR Value = 0 TO 63
                WAIT &H3DA, 8               'reduces flicker by waiting for a v. retrace
                FOR Index = 0 TO 255
                    OUT &H3C8, Index
                    FOR RGB = 1 TO 3
                        IF NewPal(Index, RGB) < Pal(Index, RGB) THEN
                            NewPal(Index, RGB) = NewPal(Index, RGB) + 1
                        END IF
                        OUT &H3C9, NewPal(Index, RGB)
                    NEXT
                NEXT
            NEXT
        CASE FadeOut
            FOR Value = 0 TO 63
                WAIT &H3DA, 8               'reduces flicker by waiting for a v. retrace
                FOR Index = 0 TO 255
                    OUT &H3C8, Index
                    FOR RGB = 1 TO 3
                        IF Pal(Index, RGB) - Value > 0 THEN
                            NewPal(Index, RGB) = Pal(Index, RGB) - Value
                        END IF
                        OUT &H3C9, NewPal(Index, RGB)
                    NEXT
                NEXT
            NEXT
    END SELECT

END SUB

FUNCTION GetName$

    DispPic 8
    Fade PopIn
   
    LOCATE 1, 3: PRINT "You made it on the high scores list!"
    LOCATE 2, 3: PRINT "Please enter your name:"
    LOCATE 3, 3: INPUT "-> ", Name$
   
    IF LEN(Name$) > 20 THEN Name$ = LEFT$(Name$, 20)
    GetName$ = Name$

END FUNCTION

SUB Help

    DispPic 4
    Fade PopIn
    WaitForKey 10000
    Fade PopOut

END SUB

SUB HighScores

    DIM Highest(10) AS Highs
    NameStr$ = "\                  \"
   
    DispPic 7
    
    OPEN ScoreFile FOR INPUT AS #1
    FOR i = 1 TO 10
        LINE INPUT #1, Highest(i).Player
        INPUT #1, Highest(i).Score
    NEXT
    CLOSE #1
   
    COLOR 1
    FOR i = 1 TO 10
        LOCATE i + 10, 3: PRINT USING NameStr$; Highest(i).Player;
        PRINT SPACE$(5);
        PRINT USING ScoreString; Highest(i).Score
    NEXT
   
    Fade PopIn
   
    WaitForKey 6000

    Fade PopOut

END SUB

SUB InitColors

    PRINT "InitColors";
    Index = 0
    FOR Value = 1 TO 28 STEP 3
        PRINT ".";
        Index = Index + 1
        PieceClr(Index) = Value
    NEXT
   
    Grey.Red = GreyValue
    Grey.Green = GreyValue
    Grey.Blue = GreyValue
    PRINT ".";

    Black.Red = 0
    Black.Green = 0
    Black.Blue = 0
    PRINT "."

END SUB

SUB Initialize
   
    RANDOMIZE TIMER
   
    PRINT "Initialize()"
   
    PRINT "+"; STRING$(47, "-"); "+"
    PRINT "|   Qtris Copyright (C) 1994, Michael Teator    |"
    PRINT "+"; STRING$(47, "-"); "+"
   
    PRINT "Reading configuration info from "; ConfigFile
    OPEN ConfigFile FOR INPUT AS #1
    LINE INPUT #1, Header$
    IF Header$ <> "[QTRIS]" THEN
        PRINT "Invalid "; ConfigFile; " file"
        StartLevel = 1
        StartLines = 0
        GameSound = True
        Fading = True
    ELSE
        INPUT #1, StartLevel, StartLines, GameSound, Fading
        END IF
    CLOSE #1

    Cols(0) = LeftMargin: Rows(1) = TopMargin

    RESTORE PicFiles
    FOR Num = 1 TO 8
        READ FileName(Num)
    NEXT
   
    FOR Level = 9 TO 39
        FileName(Level) = "Lev" + RTRIM$(LTRIM$(STR$(Level - 8)))
    NEXT

    PRINT "Cols";
    FOR Fill = 1 TO 20
        PRINT ".";
        Cols(Fill) = Cols(Fill - 1) + BoxX
    NEXT
    PRINT

    PRINT "Rows";
    FOR Fill = 2 TO 21
        PRINT ".";
        Rows(Fill) = Rows(Fill - 1) + BoxY
    NEXT
    PRINT
  
    InitColors

    InitPieces
   
    Delay 400
   
    InitVGA

END SUB

SUB InitPieces
   
    OPEN "qtris.pce" FOR BINARY AS #1
    PRINT "InitPieces";
    FOR Each = Square TO Peg
        FOR Directions = SkyUp TO SkyLeft
            FOR CellsY = 1 TO 4
                FOR CellsX = 1 TO 4
                    GET #1, , Byte
                    TetrisPieces(Each, Directions, CellsX, CellsY) = VAL(Byte)
                NEXT
            NEXT
            PRINT ".";
        NEXT
    NEXT
    PRINT
    CLOSE #1

END SUB

SUB InitVGA
 
    PRINT "InitVGA"
    OPEN "qtris.pal" FOR BINARY AS #1
    FOR Index = 0 TO 255
        FOR RGB = 1 TO 3
            GET #1, , Byte
            Pal(Index, RGB) = ASC(Byte)
        NEXT
    NEXT
    CLOSE #1
   
    SCREEN 13
    FOR Index = 0 TO 255
        OUT &H3C8, Index
        FOR RGB = 1 TO 3
            OUT &H3C9, Pal(Index, RGB)
        NEXT
    NEXT
   
END SUB

SUB Main

    CLS
    COLOR 7
   
    PRINT "Main()"
    PRINT "Free String Space:"; FRE("")
    PRINT "Free Array Space: "; FRE(-1)
    PRINT "Free Stack Space: "; FRE(-2)
    Initialize
   
    TitleScreen
   
    DO
        EndProgram = Menu   'Do it
    LOOP UNTIL EndProgram
   
    SCREEN 0                'Clean up the screen
    WIDTH 80
   
    SYSTEM

END SUB

FUNCTION Menu

    DispPic 2              'Display the menu pic
    Fade PopIn             'Pop up the pic
    Selection = 0          'init variables
    Menu = False
    y = 1
    SetColor 100 + y, Grey
   
    DO
        DO
            KeyPress$ = INKEY$
        LOOP UNTIL KeyPress$ <> ""
       
        SELECT CASE KeyPress$
            CASE CHR$(0) + CHR$(72), "8"         'Up key
                SetColor 100 + y, Black           'erase old highlight
                y = y - 1: IF y < 1 THEN y = 5   'move bar
                SetColor 100 + y, Grey            'draw new highlight
            CASE CHR$(0) + CHR$(80), "2"         'Down key
                SetColor 100 + y, Black
                y = y + 1: IF y > 5 THEN y = 1
                SetColor 100 + y, Grey
            CASE CHR$(13)                        'enter key was pressed
                Selection = y
        END SELECT
   LOOP UNTIL Selection <> 0                     'loop till the user selects somthing
  
   Fade PopOut

   SELECT CASE Selection   'what was selected?
    CASE 1
        PlayGame
        DrawFrame 106, 75, 220, 115
        COLOR 16
        LOCATE 12, 16: PRINT " GAME OVER "
        LOCATE 13, 16: PRINT "Press a key"
        PlaySound GameOver
        WaitForKey 500
        Fade FadeOut
        SaveHigh
        HighScores
    CASE 2
        HighScores
    CASE 3
        Options
    CASE 4
        Help
    CASE 5
        EndScreen
        Menu = True        'Exit the program
    END SELECT

END FUNCTION

SUB Options
   
    CONST LevelSel = 1, LinesSel = 2, SoundSel = 3, FadeSel = 4
   
    DIM Selection(4)
   
    Selection(LevelSel) = StartLevel - 1
    Selection(LinesSel) = StartLines
    Selection(SoundSel) = ABS(GameSound)
    Selection(FadeSel) = ABS(Fading)
   
    COLOR 7
    FOR Frames = 0 TO 4
    DrawFrame 0, Frames * 40, 319, 39 + Frames * 40
    NEXT
   
    LOCATE 3, 2: PRINT "Starting Level:"
    LOCATE 8, 2: PRINT "Starting Lines:"
    LOCATE 13, 2: PRINT "Game Sound:"
    LOCATE 18, 2: PRINT "Level Screen Fades:"
    LOCATE 23, 2: PRINT "Press ENTER to exit and save settings."
   
    Fade PopIn
    Current = 1

    DO
        COLOR 13
        LINE (160, 13 + 40 * (Current - 1))-(195, 26 + 40 * (Current - 1)), 4, B
      
        IF Selection(LevelSel) > 29 THEN Selection(LevelSel) = 29
        IF Selection(LinesSel) > 14 THEN Selection(LinesSel) = 14
        IF Selection(SoundSel) > 1 THEN Selection(SoundSel) = 1
        IF Selection(FadeSel) > 1 THEN Selection(FadeSel) = 1
               
        LOCATE 3, 22: PRINT USING "##"; Selection(LevelSel) + 1
        LOCATE 8, 22: PRINT USING "##"; Selection(LinesSel)
                       
        LOCATE 13, 22
        IF Selection(SoundSel) = 0 THEN
            PRINT "No "
        ELSE
            PRINT "Yes"
        END IF
                      
        LOCATE 18, 22
        IF Selection(FadeSel) = 0 THEN
            PRINT "No "
        ELSE
            PRINT "Yes"
        END IF
  
        DO
            KeyPress$ = INKEY$
        LOOP UNTIL KeyPress$ <> ""
       
        SELECT CASE KeyPress$
            CASE CHR$(0) + CHR$(75), "4"
                Selection(Current) = Selection(Current) - ABS(Selection(Current) > 0)
            CASE CHR$(0) + CHR$(77), "6"
                Selection(Current) = Selection(Current) + 1
            CASE CHR$(0) + CHR$(80), "5"
                LINE (160, 13 + 40 * (Current - 1))-(195, 26 + 40 * (Current - 1)), 0, B
                Current = Current + 1
                IF Current > 4 THEN Current = 1
            CASE CHR$(0) + CHR$(72), "8"
                LINE (160, 13 + 40 * (Current - 1))-(195, 26 + 40 * (Current - 1)), 0, B
                Current = Current - 1
                IF Current < 1 THEN Current = 4
            CASE "q", "Q", CHR$(27), CHR$(13)
                Finished = True
        END SELECT
    LOOP UNTIL Finished
   
    GameSound = Selection(SoundSel) * (-1)
    Fading = Selection(FadeSel) * (-1)
    StartLines = Selection(LinesSel)
    StartLevel = Selection(LevelSel) + 1

    OPEN ConfigFile FOR OUTPUT AS #1
        PRINT #1, "[QTRIS]"
        PRINT #1, StartLevel, StartLines, GameSound, Fading
    CLOSE #1

END SUB

SUB Pause (CurrentLevel)
   
    LINE (Cols(1), Rows(1))-(Cols(12) - 1, Rows(21) - 1), 0, BF
    COLOR 10: LOCATE 21, 31: PRINT "Paused"
    Time1! = TIMER
    KeyPressed = False
   
    DO
        Time2 = INT(TIMER - Time1!)
        IF Time2 > 10 THEN
            ScreenSaver (CurrentLevel)
            KeyPressed = True
        END IF
        IF INKEY$ <> "" THEN KeyPressed = True
    LOOP UNTIL KeyPressed
   
    LOCATE 21, 31: PRINT SPACE$(6)
    RedrawMatrix 1, 20

END SUB

SUB PlayGame
   
    DIM CurrentPiece AS Tetris, OldPiece AS Tetris
   
    IF StartLevel < 1 THEN StartLevel = 1
    IF StartLevel > 30 THEN StartLevel = 30
    Level = StartLevel
   
    ResetMatrix
   
    Dummy = ScoreSystem(ResetScore, Level)
   
    DrawScreen Level, New
   
    IF StartLines > 0 THEN PutRandomBlocks
    NextPiece = INT((RND * 7) + 1)

    DO  'Main game loop
       
        NewLines = UpDateMatrix
        Level = ScoreSystem(NewLines, Level)
        Speed = INT(((((Level - 30) ^ 2) / 30 + 1) / 30) * 10000)
        Drop = False
        Hit = False
       
        CurrentPiece.Row = 1
        CurrentPiece.Col = 5
        CurrentPiece.Sky = SkyUp
        CurrentPiece.Piece = NextPiece
        OldPiece = CurrentPiece
       
        NextPiece = INT((RND * 7) + 1)
        DrawNextPiece (NextPiece)
        
        Result = DrawPiece(CurrentPiece, OldPiece, All)
        IF NOT Result THEN EXIT SUB
       
        DO 'Piece loop
            Time1! = TIMER
           
            DO 'Frame loop
                KeyPress$ = INKEY$
               
                IF KeyPress$ <> "" THEN
                   
                    SELECT CASE KeyPress$
                        CASE CHR$(0) + CHR$(75), "4"
                            OldPiece.Col = CurrentPiece.Col
                            CurrentPiece.Col = CurrentPiece.Col - 1
                        CASE CHR$(0) + CHR$(77), "6"
                            OldPiece.Col = CurrentPiece.Col
                            CurrentPiece.Col = CurrentPiece.Col + 1
                        CASE CHR$(0) + CHR$(72), "8"
                            OldPiece.Sky = CurrentPiece.Sky
                            CurrentPiece.Sky = CurrentPiece.Sky + 1
                            IF CurrentPiece.Sky > SkyLeft THEN CurrentPiece.Sky = SkyUp
                            PlaySound Rotate
                        CASE CHR$(0) + CHR$(80), "5"
                            Drop = True
                        CASE "s", "S"
                            Dummy = DrawPiece(CurrentPiece, OldPiece, OnlyErase)
                            SWAP CurrentPiece.Piece, NextPiece
                            Result = DrawPiece(CurrentPiece, OldPiece, All)
                            IF Result THEN
                                DrawNextPiece (NextPiece)
                                PlaySound Swapped
                            ELSE
                                SWAP CurrentPiece.Piece, NextPiece
                                Dummy = DrawPiece(CurrentPiece, OldPiece, All)
                                PlaySound HitWall
                            END IF
                        CASE "p", "P"
                            TimeTemp! = TIMER - Time1!
                            Pause Level
                            DrawNextPiece (NextPiece)
                            Dummy = ScoreSystem(0, Level)
                            Time1! = TIMER + TimeTemp!
                        CASE "q", "Q", CHR$(27)
                            EXIT SUB
                    END SELECT
                   
                    Result = DrawPiece(CurrentPiece, OldPiece, All)
                   
                    IF Result THEN
                        OldPiece.Col = CurrentPiece.Col
                        OldPiece.Sky = CurrentPiece.Sky
                    ELSE
                        CurrentPiece.Col = OldPiece.Col
                        CurrentPiece.Sky = OldPiece.Sky
                        PlaySound HitWall
                    END IF
               
                END IF
               
                Time2 = INT((TIMER - Time1!) * 10000)
           
            LOOP UNTIL (Time2 > Speed) OR Drop  'Frame Loop
           
            CurrentPiece.Row = CurrentPiece.Row + 1
            Result = DrawPiece(CurrentPiece, OldPiece, All)
            
            IF Result THEN
                OldPiece.Row = CurrentPiece.Row
            ELSE
                Hit = True
                PlaySound HitBottom
            END IF
       
        LOOP UNTIL Hit  'Piece Loop
   
    LOOP    'Main game loop

END SUB

SUB PlaySound (WhichSound)

    IF NOT GameSound THEN EXIT SUB
    SELECT CASE WhichSound
        CASE Rotate
            FOR i = 50 TO 500 STEP 10
                SOUND i, .05
            NEXT
        CASE HitBottom
            SOUND 50, .8
        CASE HitWall
            SOUND 500, .1
        CASE GotTetris
            FOR i = 1 TO 10
                z = INT(RND * 200) + 100
                SOUND z, .5
            NEXT
        CASE GameOver
            SOUND 150, 5
            SOUND 50, 5
        CASE Swapped
            FOR i = 50 TO 1000 STEP 10
                SOUND i, .05
            NEXT
            FOR i = 1000 TO 500 STEP -10
                SOUND i, .05
            NEXT
    END SELECT

END SUB

SUB PrintScore (Player AS Score)

    COLOR 1: LOCATE 13, 31: PRINT USING ScoreString; Player.Lines
    COLOR 4: LOCATE 16, 31: PRINT USING ScoreString; Player.Level
    COLOR 7: LOCATE 19, 31: PRINT USING ScoreString; Player.Points

END SUB

SUB PutRandomBlocks

    FOR y = 20 - StartLines + 1 TO 20
        FOR x = 1 TO 11
            IF RND > .666 THEN   'seemed like a good evil number
                DrawBox x, y, PieceClr(8), DrawP, Moving
            END IF
        NEXT
    NEXT
    Dummy = UpDateMatrix

END SUB

SUB RedrawMatrix (RowStart, RowEnd)

    FOR y = RowStart TO RowEnd
        FOR x = 1 TO 11
            IF ScreenMatrix(x, y) > 0 THEN
                DrawBox x, y, (ScreenMatrix(x, y) - 32), DrawP, ScreenMatrix(x, y)
            ELSE
                DrawBox x, y, 0, EraseP, NoBlock
            END IF
        NEXT
    NEXT

END SUB

SUB ResetMatrix
   
    FOR y = 1 TO 20
        FOR x = 1 TO 11
            ScreenMatrix(x, y) = 0
        NEXT
    NEXT

END SUB

SUB RotatePalette (PalFile AS STRING)


    DIM RotatedPal(-1 TO 255, 3)

    OPEN PalFile FOR BINARY AS #1     'read in the palette to be rotated
        FOR Index = 0 TO 255
            FOR RGB = 1 TO 3
                GET #1, , Byte: RotatedPal(Index, RGB) = ASC(Byte)
            NEXT
        NEXT
    CLOSE #1
   
    DO
        FOR Shift = -1 TO 254
            FOR RGB = 1 TO 3
                RotatedPal(Shift, RGB) = RotatedPal(Shift + 1, RGB)
            NEXT
        NEXT
        FOR LoopedPal = 1 TO 3
            RotatedPal(255, LoopedPal) = RotatedPal(-1, LoopedPal)
        NEXT
        WAIT &H3DA, 8               'reduces flicker by waiting for a v. retrace
        FOR Index = 1 TO 255
            OUT &H3C8, Index
            FOR RGB = 1 TO 3
                OUT &H3C9, RotatedPal(Index, RGB)
            NEXT
        NEXT
    LOOP UNTIL INKEY$ <> ""

    FOR Index = 0 TO 255  'restore the old palette
        OUT &H3C8, Index
        FOR RGB = 1 TO 3
            OUT &H3C9, Pal(Index, RGB)
        NEXT
    NEXT

END SUB

SUB SaveHigh

    DIM Highest(10) AS Highs
   
    Points = ScoreSystem(FinalScore, 0)
    OPEN ScoreFile FOR INPUT AS #1
    FOR i = 1 TO 10
        LINE INPUT #1, Highest(i).Player
        INPUT #1, Highest(i).Score
    NEXT
    CLOSE #1
   
    Sort Highest()
   
    IF Points > Highest(10).Score THEN
        Highest(10).Score = Points
        Sort Highest()
        Location = Search(Highest(), Points)
        Highest(Location).Player = GetName$
    END IF
   
    OPEN ScoreFile FOR OUTPUT AS #1
    FOR i = 1 TO 10
        PRINT #1, Highest(i).Player
        PRINT #1, Highest(i).Score
    NEXT
    CLOSE #1

END SUB

FUNCTION ScoreSystem (NewLines, Level) STATIC

    DIM Player AS Score
   
    IF NewLines = ResetScore THEN
        NewLines = 0
        Player.Points = 0
        Player.Lines = 0
        OldLines = 0
    ELSEIF NewLines = FinalScore THEN
        ScoreSystem = Player.Points
        EXIT FUNCTION
    END IF

    Player.Level = Level
    Player.Lines = Player.Lines + NewLines

    IF (Player.Lines - 10 >= OldLines) AND (Player.Level < 30) THEN
        Player.Level = Player.Level + 1
        OldLines = OldLines + 10
        IF Fading THEN
            Fade FadeOut
        ELSE
            Fade PopOut
        END IF
        DrawScreen Player.Level, New
        RedrawMatrix 1, 20
    END IF
   
    Player.Points = Player.Points + NewLines * 10 - (NewLines - 1) * (NewLines - 1 > 0) * 10
   
    PrintScore Player
   
    ScoreSystem = Player.Level

END FUNCTION

SUB ScreenSaver (CurrentLevel)

    DispPic 6
    RotatePalette ScreenSaverPal
    DrawScreen CurrentLevel, Redraw

END SUB

FUNCTION Search (Highest() AS Highs, Goal)

    FOR i = 1 TO 10
        IF Highest(i).Score = Goal THEN
            Search = i
            EXIT FUNCTION
        END IF
    NEXT

END FUNCTION

SUB SetColor (Index, NewClr AS VGA)
       
        OUT &H3C8, Index        'tell the video card what color you want to set
        OUT &H3C9, NewClr.Red
        OUT &H3C9, NewClr.Green 'Set each RGB value
        OUT &H3C9, NewClr.Blue
       
END SUB

SUB Sort (Highest() AS Highs)

    FOR Last = 10 TO 2 STEP -1
        FOR Current = 1 TO Last - 1
            IF Highest(Current).Score < Highest(Current + 1).Score THEN SWAP Highest(Current), Highest(Current + 1)
        NEXT
    NEXT

END SUB

FUNCTION TestPiece (Piece AS Tetris)
           
    TestPiece = True
    FOR y = 1 TO 4
        FOR x = 1 TO 4
            IF (TetrisPieces(Piece.Piece, Piece.Sky, x, y) = 1) AND ((ScreenMatrix(Piece.Col + x - 1, Piece.Row + y - 1) > 32) OR (ScreenMatrix(Piece.Col + x - 1, Piece.Row + y - 1) = WallPiece)) THEN
                TestPiece = False
                EXIT FUNCTION
            END IF
        NEXT
    NEXT

END FUNCTION

SUB TitleScreen
   
    DispPic 1
    Fade FadeIn
    WaitForKey 500
    Fade FadeOut

END SUB

FUNCTION UpDateMatrix

    DIM LineArray(20)
    DIM ScreenTemp(11, -4 TO 20)
   
    Start = 0

    FOR y = 1 TO 20
        FullLine = True
        FOR x = 1 TO 11
            IF (ScreenMatrix(x, y) > 0) AND (ScreenMatrix(x, y) < 26) THEN
                ScreenMatrix(x, y) = ScreenMatrix(x, y) + 32
            ELSEIF ScreenMatrix(x, y) = 0 THEN
                FullLine = False
            END IF
        NEXT
        IF FullLine THEN
            LineArray(y) = 1
            IF Start = 0 THEN Start = y
        END IF
    NEXT

    IF Start <> 0 THEN
        FOR y = Start TO 20
            IF LineArray(y) = 1 THEN
                FOR x = 1 TO 11
                    DrawBox x, y, PieceClr(10), DrawP, NoBlock
                NEXT
                Lines = Lines + 1
            END IF
        NEXT
        IF Lines = 4 THEN PlaySound GotTetris
        Delay 15
        Lines = 0
        FOR y = Start TO 20
            IF LineArray(y) = 1 THEN
                FOR x = 1 TO 11
                    DrawBox x, y, 0, DrawP, DeadBlock
                NEXT
                Lines = Lines + 1
            END IF
        NEXT

        FOR n = 1 TO Lines
            FOR y = 1 TO 20
                FOR x = 1 TO 11
                    ScreenTemp(x, y) = ScreenMatrix(x, y)
                NEXT
            NEXT
            FOR y = 1 TO 20
                IF ScreenMatrix(1, y) <> DeadBlock THEN
                    FOR x = 1 TO 11
                        ScreenMatrix(x, y) = ScreenTemp(x, y - 1)
                    NEXT
                ELSE
                    FOR x = 1 TO 11
                        ScreenMatrix(x, y) = ScreenTemp(x, y - 1)
                    NEXT
                    StopDraw = y
                    EXIT FOR
                END IF
            NEXT
            RedrawMatrix 1, StopDraw
            Delay 10
        NEXT

    END IF
    UpDateMatrix = Lines

END FUNCTION

SUB WaitForKey (HundSec AS SINGLE)
   
    HundSec = HundSec / 100
    Time1! = TIMER
    DO
        Time2! = TIMER - Time1!
    LOOP UNTIL (Time2! > HundSec) OR (INKEY$ <> "")

END SUB

