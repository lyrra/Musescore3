;;   internal
;;   The value of this enum determines the "stacking order"
;;   of elements on the canvas.
;;   Note: keep in sync with array elementNames[] in scoreElement.cpp
(define-c-type (
  (name        . ElementType)
  (c-type      . "ElementType")
  (types       . (

      INVALID
      BRACKET_ITEM
      PART
      STAFF
      SCORE
      SYMBOL
      TEXT
      MEASURE_NUMBER
      MMREST_RANGE
      INSTRUMENT_NAME
      SLUR_SEGMENT
      TIE_SEGMENT
      BAR_LINE
      STAFF_LINES
      SYSTEM_DIVIDER
      STEM_SLASH
      ARPEGGIO
      ACCIDENTAL
      LEDGER_LINE
      STEM                    ; list STEM before NOTE: notes in TAB might 'break' stems
      NOTE                    ; and this requires stems to be drawn before notes
      CLEF                    ; elements from CLEF to TIMESIG need to be in the order
      KEYSIG                  ; in which they appear in a measure
      AMBITUS
      TIMESIG
      REST
      BREATH
      REPEAT_MEASURE
      TIE
      ARTICULATION
      FERMATA
      CHORDLINE
      DYNAMIC
      BEAM
      HOOK
      LYRICS
      FIGURED_BASS
      MARKER
      JUMP
      FINGERING
      TUPLET
      TEMPO_TEXT
      STAFF_TEXT
      SYSTEM_TEXT
      REHEARSAL_MARK
      INSTRUMENT_CHANGE
      STAFFTYPE_CHANGE
      HARMONY
      FRET_DIAGRAM
      BEND
      TREMOLOBAR
      VOLTA
      HAIRPIN_SEGMENT
      OTTAVA_SEGMENT
      TRILL_SEGMENT
      LET_RING_SEGMENT
      VIBRATO_SEGMENT
      PALM_MUTE_SEGMENT
      TEXTLINE_SEGMENT
      VOLTA_SEGMENT
      PEDAL_SEGMENT
      LYRICSLINE_SEGMENT
      GLISSANDO_SEGMENT
      LAYOUT_BREAK
      SPACER
      STAFF_STATE
      NOTEHEAD
      NOTEDOT
      TREMOLO
      IMAGE
      MEASURE
      SELECTION
      LASSO
      SHADOW_NOTE
      TAB_DURATION_SYMBOL
      FSYMBOL
      PAGE
      HAIRPIN
      OTTAVA
      PEDAL
      TRILL
      LET_RING
      VIBRATO
      PALM_MUTE
      TEXTLINE
      TEXTLINE_BASE
      NOTELINE
      LYRICSLINE
      GLISSANDO
      BRACKET
      SEGMENT
      SYSTEM
      COMPOUND
      CHORD
      SLUR
      ELEMENT
      ELEMENT_LIST
      STAFF_LIST
      MEASURE_LIST
      HBOX
      VBOX
      TBOX
      FBOX
      ICON
      OSSIA
      BAGPIPE_EMBELLISHMENT
      STICKING

      MAXTYPE))))

;;;---------------------------------------------------------
;;;  AccidentalType
;;;  NOTE: keep this in sync with with accList array in accidentals.cpp
;;;---------------------------------------------------------
(define-c-type (
  (name   . AccidentalType)
  (c-type . "AccidentalType")
  (types . (
    NONE
    FLAT
    NATURAL
    SHARP
    SHARP2
    FLAT2
    SHARP3
    FLAT3
    NATURAL_FLAT
    NATURAL_SHARP
    SHARP_SHARP

    ; Gould arrow quartertone
    FLAT_ARROW_UP
    FLAT_ARROW_DOWN
    NATURAL_ARROW_UP
    NATURAL_ARROW_DOWN
    SHARP_ARROW_UP
    SHARP_ARROW_DOWN
    SHARP2_ARROW_UP
    SHARP2_ARROW_DOWN
    FLAT2_ARROW_UP
    FLAT2_ARROW_DOWN
    ARROW_DOWN
    ARROW_UP

    ; Stein-Zimmermann
    MIRRORED_FLAT
    MIRRORED_FLAT2
    SHARP_SLASH
    SHARP_SLASH4

    ; Arel-Ezgi-Uzdilek (AEU)
    FLAT_SLASH2
    FLAT_SLASH
    SHARP_SLASH3
    SHARP_SLASH2

    ; Extended Helmholtz-Ellis accidentals (just intonation)
    DOUBLE_FLAT_ONE_ARROW_DOWN
    FLAT_ONE_ARROW_DOWN
    NATURAL_ONE_ARROW_DOWN
    SHARP_ONE_ARROW_DOWN
    DOUBLE_SHARP_ONE_ARROW_DOWN
    DOUBLE_FLAT_ONE_ARROW_UP

    FLAT_ONE_ARROW_UP
    NATURAL_ONE_ARROW_UP
    SHARP_ONE_ARROW_UP
    DOUBLE_SHARP_ONE_ARROW_UP
    DOUBLE_FLAT_TWO_ARROWS_DOWN
    FLAT_TWO_ARROWS_DOWN

    NATURAL_TWO_ARROWS_DOWN
    SHARP_TWO_ARROWS_DOWN
    DOUBLE_SHARP_TWO_ARROWS_DOWN
    DOUBLE_FLAT_TWO_ARROWS_UP
    FLAT_TWO_ARROWS_UP
    NATURAL_TWO_ARROWS_UP

    SHARP_TWO_ARROWS_UP
    DOUBLE_SHARP_TWO_ARROWS_UP
    DOUBLE_FLAT_THREE_ARROWS_DOWN
    FLAT_THREE_ARROWS_DOWN
    NATURAL_THREE_ARROWS_DOWN
    SHARP_THREE_ARROWS_DOWN

    DOUBLE_SHARP_THREE_ARROWS_DOWN
    DOUBLE_FLAT_THREE_ARROWS_UP
    FLAT_THREE_ARROWS_UP
    NATURAL_THREE_ARROWS_UP
    SHARP_THREE_ARROWS_UP
    DOUBLE_SHARP_THREE_ARROWS_UP

    LOWER_ONE_SEPTIMAL_COMMA
    RAISE_ONE_SEPTIMAL_COMMA
    LOWER_TWO_SEPTIMAL_COMMAS
    RAISE_TWO_SEPTIMAL_COMMAS
    LOWER_ONE_UNDECIMAL_QUARTERTONE
    RAISE_ONE_UNDECIMAL_QUARTERTONE

    LOWER_ONE_TRIDECIMAL_QUARTERTONE
    RAISE_ONE_TRIDECIMAL_QUARTERTONE

    DOUBLE_FLAT_EQUAL_TEMPERED
    FLAT_EQUAL_TEMPERED
    NATURAL_EQUAL_TEMPERED
    SHARP_EQUAL_TEMPERED
    DOUBLE_SHARP_EQUAL_TEMPERED
    QUARTER_FLAT_EQUAL_TEMPERED
    QUARTER_SHARP_EQUAL_TEMPERED

    FLAT_17
    SHARP_17
    FLAT_19
    SHARP_19
    FLAT_23
    SHARP_23
    FLAT_31
    SHARP_31
    FLAT_53
    SHARP_53
    ; EQUALS_ALMOST
    ; EQUALS
    ; TILDE

    ; Persian
    SORI
    KORON

    ; Wyschnegradsky
    TEN_TWELFTH_FLAT
    TEN_TWELFTH_SHARP
    ELEVEN_TWELFTH_FLAT
    ELEVEN_TWELFTH_SHARP
    ONE_TWELFTH_FLAT
    ONE_TWELFTH_SHARP
    TWO_TWELFTH_FLAT
    TWO_TWELFTH_SHARP
    THREE_TWELFTH_FLAT
    THREE_TWELFTH_SHARP
    FOUR_TWELFTH_FLAT
    FOUR_TWELFTH_SHARP
    FIVE_TWELFTH_FLAT
    FIVE_TWELFTH_SHARP
    SIX_TWELFTH_FLAT
    SIX_TWELFTH_SHARP
    SEVEN_TWELFTH_FLAT
    SEVEN_TWELFTH_SHARP
    EIGHT_TWELFTH_FLAT
    EIGHT_TWELFTH_SHARP
    NINE_TWELFTH_FLAT
    NINE_TWELFTH_SHARP

    ; (Spartan) Sagittal
    SAGITTAL_5V7KD
    SAGITTAL_5V7KU
    SAGITTAL_5CD
    SAGITTAL_5CU
    SAGITTAL_7CD
    SAGITTAL_7CU
    SAGITTAL_25SDD
    SAGITTAL_25SDU
    SAGITTAL_35MDD
    SAGITTAL_35MDU
    SAGITTAL_11MDD
    SAGITTAL_11MDU
    SAGITTAL_11LDD
    SAGITTAL_11LDU
    SAGITTAL_35LDD
    SAGITTAL_35LDU
    SAGITTAL_FLAT25SU
    SAGITTAL_SHARP25SD
    SAGITTAL_FLAT7CU
    SAGITTAL_SHARP7CD
    SAGITTAL_FLAT5CU
    SAGITTAL_SHARP5CD
    SAGITTAL_FLAT5V7KU
    SAGITTAL_SHARP5V7KD
    SAGITTAL_FLAT
    SAGITTAL_SHARP

    ; Turkish folk music
    ONE_COMMA_FLAT
    ONE_COMMA_SHARP
    TWO_COMMA_FLAT
    TWO_COMMA_SHARP
    THREE_COMMA_FLAT
    THREE_COMMA_SHARP
    FOUR_COMMA_FLAT
    ; FOUR_COMMA_SHARP
    FIVE_COMMA_SHARP

    END))))


(define-c-type (
  (name        . NoteType)
  (c-type      . "NoteType")
  (types       . (
    (NORMAL          0)
    (ACCIACCATURA    #x1)
    (APPOGGIATURA    #x2)  ; grace notes
    (GRACE4          #x4)
    (GRACE16         #x8)
    (GRACE32         #x10)
    (GRACE8_AFTER    #x20)
    (GRACE16_AFTER   #x40)
    (GRACE32_AFTER   #x80)
    (INVALID         #xFF)))))

(define-c-type (
  (name   . Direction)
  (c-type . "Direction")
  (types  . (
    AUTO
    UP
    DOWN))))

;---------------------------------------------------------
;   GlissandoType
;---------------------------------------------------------
(define-c-type (
  (name        . GlissandoType)
  (c-type      . "GlissandoType")
  (types       . (
      STRAIGHT WAVY))))

;---------------------------------------------------------
;   GlissandoStyle
;---------------------------------------------------------
(define-c-type (
  (name        . GlissandoStyle)
  (c-type      . "GlissandoStyle")
  (types       . (
      CHROMATIC WHITE_KEYS BLACK_KEYS DIATONIC PORTAMENTO))))

;---------------------------------------------------------
;   HarmonyType
;---------------------------------------------------------
(define-c-type (
  (name        . HarmonyType)
  (c-type      . "HarmonyType")
  (types       . (
      STANDARD
      ROMAN
      NASHVILLE))))

;---------------------------------------------------------
;   Placement
;---------------------------------------------------------
(define-c-type (
  (name        . Placement)
  (c-type      . "Placement")
  (types       . (
      ABOVE BELOW))))

;---------------------------------------------------------
;   HPlacement
;---------------------------------------------------------
(define-c-type (
  (name        . HPlacement)
  (c-type      . "HPlacement")
  (types       . (
      LEFT CENTER RIGHT))))

;---------------------------------------------------------
;   MMRestRangeBracketType
;---------------------------------------------------------
(define-c-type (
  (name        . MMRestRangeBracketType)
  (c-type      . "MMRestRangeBracketType")
  (types       . (
      BRACKETS PARENTHESES NONE))))

;---------------------------------------------------------
;   OffsetType
;---------------------------------------------------------
(define-c-type (
  (name        . OffsetType)
  (c-type      . "OffsetType")
  (datatype . "char")
  (types       . (
      ABS       ; offset in point units
      SPATIUM   ; offset in staff space units
      ))))

;; SegmentType
;; Type values determine the order of segments for a given tick
(define-c-type (
  (name        . SegmentType)
  (c-type      . "SegmentType")
  (types       . (
    (Invalid              #x0)
    (BeginBarLine         #x1)
    (HeaderClef           #x2)
    (KeySig               #x4)
    (Ambitus              #x8)
    (TimeSig              #x10)
    (StartRepeatBarLine   #x20)
    (Clef                 #x40)
    (BarLine              #x80)
    (Breath               #x100)
    (ChordRest            #x200)
    (EndBarLine           #x400)
    (KeySigAnnounce       #x800)
    (TimeSigAnnounce      #x1000)
    (All                  -1) ; Includes all barline types
    ;(BarLineType #x4a1)
    (BarLineType (or BeginBarLine StartRepeatBarLine BarLine EndBarLine))
    ))))

;   Tid
;   Enumerates the list of built-in text substyles
;   internal
;   Must be in sync with textStyles array (in style.cpp)
(define-c-type (
  (name        . Tid)
  (c-type      . "Tid")
  (types       . (
      DEFAULT
      TITLE
      SUBTITLE
      COMPOSER
      POET
      TRANSLATOR
      FRAME
      INSTRUMENT_EXCERPT
      INSTRUMENT_LONG
      INSTRUMENT_SHORT
      INSTRUMENT_CHANGE
      HEADER
      FOOTER
      MEASURE_NUMBER
      MMREST_RANGE
      TEMPO
      METRONOME
      REPEAT_LEFT       ; align to start of measure
      REPEAT_RIGHT      ; align to end of measure
      REHEARSAL_MARK
      SYSTEM
      STAFF
      EXPRESSION
      DYNAMICS
      HAIRPIN
      LYRICS_ODD
      LYRICS_EVEN
      HARMONY_A
      HARMONY_B
      HARMONY_ROMAN
      HARMONY_NASHVILLE
      TUPLET
      STICKING
      FINGERING
      LH_GUITAR_FINGERING
      RH_GUITAR_FINGERING
      STRING_NUMBER
      TEXTLINE
      VOLTA
      OTTAVA
      GLISSANDO
      PEDAL
      BEND
      LET_RING
      PALM_MUTE
      USER1
      USER2
      USER3
      USER4
      USER5
      USER6
      USER7
      USER8
      USER9
      USER10
      USER11
      USER12
      ; special, no-contents, styles used while importing older scores
      TEXT_STYLES            ; used for user-defined styles
      IGNORED_STYLES         ; used for styles no longer relevant (mainly Figured bass text style)
      ))))

;---------------------------------------------------------
;   Align
;   Because the Align enum has Top = 0 and Left = 0,
;   align() & Align::Top will always return false.
;   @warning Do not use if (align() & Align::Top) { doSomething(); }
;   because doSomething() will never be executed!
;   use this instead:
;   `if ((static_cast<char>(align()) & static_cast<char>(Align::VMASK)) == Align::Top) { doSomething(); }`
;   Same applies to Align::Left.
;---------------------------------------------------------
(define-c-type (
  (name        . Align)
  (c-type      . "Align")
  (datatype . "char")
  (types       . (
    (LEFT      0)
    (RIGHT     1)
    (HCENTER   2)
    (TOP       0)
    (BOTTOM    4)
    (VCENTER   8)
    (BASELINE 16)
    (CENTER (or HCENTER VCENTER))
    (HMASK  (or LEFT RIGHT    HCENTER))
    (VMASK  (or TOP  BOTTOM   VCENTER BASELINE))))))

;---------------------------------------------------------
;   FontStyle
;---------------------------------------------------------
(define-c-type (
  (name        . FontStyle)
  (c-type      . "FontStyle")
  (datatype . "char")
  (types       . (
    Normal
    (Bold 1)
    (Italic 2)
    (Underline 4)
    (Strike 8)))))

;---------------------------------------------------------
;   PlayEventType
; Determines whether oranaments are automatically generated
; when playing a score and whether the PlayEvents are saved
; in the score file.
;---------------------------------------------------------
(define-c-type (
  (name        . PlayEventType)
  (c-type      . "PlayEventType")
  (datatype . "char")
  (types       . (
    Auto        ; Play events for all notes are calculated by MuseScore.
    User     ; Some play events are modified by user. Those events are written into the mscx file.
    ))))

;---------------------------------------------------------
;   Tuplets
;---------------------------------------------------------
(define-c-type (
  (name        . TupletNumberType)
  (c-type      . "TupletNumberType")
  (datatype . "char")
  (types       . (SHOW_NUMBER SHOW_RELATION NO_TEXT))))
(define-c-type (
  (name        . TupletBracketType)
  (c-type      . "TupletBracketType")
  (datatype . "char")
  (types       . (AUTO_BRACKET SHOW_BRACKET SHOW_NO_BRACKET))))
