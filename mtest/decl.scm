
;
; initiate c and h file
;

(define (string-lisp->c str)
  (let ((idx (string-position "-" str)))
    (if idx
        (string-lisp->c
         (string-append (substring str 0 idx)
                        "__"
                        (substring str (+ 1 idx))))
        str)))

(format %h "// WARNING: this file is generated~%")
(format %c "// WARNING: this file is generated
#include <cstdlib>
#include <cstdio>
#include \"s7/s7.h\"
#include \"s7/utils.h\"

#include <QtTest/QtTest>
#include \"musescore-qt.h\"
#include \"libmscore/score.h\"
#include \"libmscore/undo.h\"
#include \"libmscore/measure.h\"
#include \"libmscore/element.h\"
#include \"libmscore/tremolo.h\"
#include \"libmscore/accidental.h\"
#include \"libmscore/articulation.h\"
#include \"libmscore/sym.h\"
#include \"libmscore/hairpin.h\"
#include \"libmscore/breath.h\"
#include \"mtest/testutils.h\"
#include \"s7gen.h\"

const char* my_s7_get_car_as_string (s7_pointer lst);

using namespace Ms;

extern Ms::MTest* g_mtest;
")

(format %h "void init_gen_s7 (s7_scheme *sc);~%~%")

;
;
;

(register-c-type %symid)
(emit-c-type-string-maps3 'SymId)

(emit-c-type-string-maps3 'Sid)

(evalc
  `(defcenum GOO_TYPE "uint64_t"
             ,(append %goo-types
                      (map (lambda (type)
                             (let ((name (car type)))
                               (string-lisp->c (symbol->string name))))
                           (cdr (assoc 'ElementType %c-types))))))

(evalc
  (defcreg ('ms-make-nullgoo) ()
    '(c_make_goo sc 0 (s7_f sc) nullptr)))

;
;
;
(newline %h)
(register-c-type %directionh)
(register-c-type %note-value-type)
(register-c-type %note-head-type)
(register-c-type %note-head-group)
(emit-c-type-string-maps3 'DirectionH)
(emit-c-type-string-maps3 'Direction)
(emit-c-type-string-maps3 'note_ValueType)
(emit-c-type-string-maps3 'NoteHead_Type)
(emit-c-type-string-maps3 'NoteHead_Group)
(emit-c-type-string-maps-simple "element_pid" (cdr (assoc 'Pid %c-types)) "Ms::Pid")

(emit-c-type-string-maps3 'NoteType)

(defcompile
 (defcreg ('ms-make-tduration) (1)
   (emit-pop-arg-sym '("dur"))
   '(c_make_goo sc
                "static_cast<uint64_t>(GOO_TYPE::CHORD)"
                (s7_nil sc)
                "(void*) new TDuration(string_to_DurationType(dur))")))

(defcompile
 (defcreg ('ms-make-score) ()
   '(c_make_goo sc
                "static_cast<uint64_t>(0 /*GOO_TYPE::CHORD*/)"
                (s7_nil sc)
                "new Ms::Score()")))

(defcompile
 (defcreg ('ms-make-chord) ()
   '(c_make_goo sc
                "static_cast<uint64_t>(GOO_TYPE::CHORD)"
                (s7_nil sc)
                "new Ms::Chord(g_mtest->score)")))

(defcompile
 (defcreg ('ms-make-note) ()
   '(c_make_goo sc
                "static_cast<uint64_t>(GOO_TYPE::NOTE)"
                (s7_nil sc)
                "new Ms::Note(g_mtest->score)")))

;
; (ms-make-element <element-name>) => element-object
;
(defcompile
 (defcreg ('ms-make-element) (1)
   '(if (! (s7_is_symbol (s7_car args)))
        (return (s7_wrong_type_arg_error sc "\"ms_make_element\"" 1 (s7_car args) "\"an symbol\"")))
   '(raw "const char *symname = s7_symbol_name(s7_car(args))")
   '(raw "uint64_t ty")
   '(raw "Element* e = NULL")
   (append
      '(cond)
      (map (lambda (type)
             (let ((name (car type))
                   (cname (cadr type)))
               (list `(! (strcasecmp symname ,(format #f "\"~a\"" name)))
                    (list 'raw (format #f "e = Element::create(~a, g_mtest->score)" cname))
                    (list 'raw (format #f "ty = static_cast<uint64_t>(GOO_TYPE::~a)"
                                       (string-lisp->c (symbol->string name)))))))
           (cdr (assoc 'ElementType %c-types)))
      (list
      (list 'else
            (list 'raw (format #f "fprintf(stderr, \"ERROR: UNRECOGNIZED ELEMENT TYPE: %s\\n\", symname)"))
            '(return (s7_nil sc)))))
   '(c_make_goo sc ty (s7_f sc) e)))


(defcompile
 (defcreg ('ms-element-type) (1)
   (emit-pop-arg-goo '("Element*" "e"))
   '(s7_make_integer sc "static_cast<uint64_t>(e->type())")))

(def-goo-setters-goo "Ms::Element*" "element" "parent" "setParent" "Element*")

(defcompile
  (defcreg ('ms-score-selection-elements) (1)
    (emit-pop-arg-goo '("MasterScore*" "score"))
    '(raw "const void *elms = &(score->selection().elements())")
    (emit-return-goo "elms" 0)))


(defcompile
  (defcreg ('ms-elements-getnext) (1)
    (emit-pop-arg-goo '("QList<Element*>*" "elms"))
    '(raw "int cnt = 0")
    '(when (&& g->data (s7_is_integer g->data))
      (raw "cnt = s7_integer(g->data)")
      (raw "cnt++"))
    '(if (>= cnt (elms->size)) ; no more elements
      (return (s7_f sc)))
    '(raw "g->data = s7_make_integer(sc, cnt)")
    '(raw "Element* elm = elms->at(cnt)")
    (emit-return-goo "elm" 0)))

;
; breath
;
(defcompile
  (defcreg ('ms-make-breath) (1)
    (emit-pop-arg-goo '("MasterScore*" "score"))
    '(if (! score) (return (s7_f sc)))
    '(raw "Breath* b = new Breath(score);")
    '(raw "uint64_t ty = 0;")
    '(c_make_goo sc ty (s7_f sc) b)))

(defcompile
  (defcreg ('ms-make-editdata) ()
    '(raw "EditData* ed = new EditData(0);")
    '(raw "ed->view = 0;")
    '(raw "uint64_t ty = 0;")
    '(c_make_goo sc ty (s7_f sc) ed)))

(defcompile
  (defcreg ('ms-editdata-dropelement) (2)
    (emit-pop-arg-goo '("_"))
    (emit-next-arg)
    '(raw "s = s7_car(args);")
    '(if (! (c_is_goo sc s)) (return (s7_f sc)))
    '(raw "goo_t *g2 = (goo_t *)s7_c_object_value(s);")
    '(raw "EditData* ed = (EditData*) g->cd;")
    '(raw "ed->dropElement = (Element *) g2->cd;")
    '(s7_t sc)))

(defcompile
  (defcreg ('ms-element-drop) (2)
    (emit-pop-arg-goo '())
    (emit-next-arg)
    '(raw "s = s7_car(args);")
    '(if (! (c_is_goo sc s)) (return (s7_f sc)))
    '(raw "goo_t *g2 = (goo_t *)s7_c_object_value(s);")
    '(raw "Element *e = (Element*) g->cd;")
    '(raw "EditData* ed = (EditData*) g2->cd;")
    '(cond
     ((e->acceptDrop *ed)
      (e->drop *ed)
      (s7_t sc))
     (else
      (s7_f sc)))))

;
; hairpin
;

(register-c-type %hairpin-type)
(emit-c-type-string-maps3 'hairpin)

(defcompile
  (defcreg ('ms-make-hairpin) ()
    '(raw "Hairpin* hp = new Hairpin(g_mtest->score);")
    (emit-return-goo "hp" "static_cast<uint64_t>(GOO_TYPE::ElementType__HAIRPIN)")))

(def-goo-setters-sym "Ms::Hairpin" "hairpin" "hairpinType" "setHairpinType" "hairpin")

; tremolo
(emit-c-type-string-maps3 'TremoloType)

(defcompile
  (defcreg ('ms-make-tremolo) (1)
    (emit-pop-arg-goo '("MasterScore*" "score"))
    '(raw "Tremolo* tr = new Tremolo(score)")
    (emit-return-goo "tr" "static_cast<uint64_t>(GOO_TYPE::ElementType__TREMOLO)")))

(def-goo-setters-sym "Ms::Tremolo" "tremolo" "tremoloType" "setTremoloType" "TremoloType")

; articulation

(defcompile
  (defcreg ('ms-make-articulation) (2)
    (emit-pop-arg-goo '("MasterScore*" "score"))
    (emit-next-arg)
    (emit-pop-arg-sym '("sym"))
    '(raw "Ms::SymId syid = string_to_SymId(sym)")
    '(raw "Articulation* ar = new Articulation(syid, score)")
    (emit-return-goo "ar" "static_cast<uint64_t>(0)")))

;
; note
;

(defcompile
  (defcreg ('ms-note-usermirror) (1)
    (emit-pop-arg-goo '("Ms::Note*" "note"))
    '(s7_make_symbol sc (DirectionH_to_string (note->userMirror)))))

(defcompile
  (defcreg ('ms-set-note-usermirror) (2)
    (emit-pop-arg-goo '("Ms::Note*" "note"))
    (emit-next-arg)
    (emit-pop-arg-sym '("symname"))
    '(note->setUserMirror (string_to_DirectionH symname))
    's))

(defcompile
  (defcreg ('ms-note-set-property) (3)
    (emit-pop-arg-goo '("Ms::Note*" "note"))
    '(raw "s7_pointer sym = s7_car(s7_cdr(args))")
    '(raw "s7_pointer val = s7_car(s7_cddr(args))")
    '(cond
      ((s7_is_boolean val)
       (note->setProperty (string_to_element_pid (s7_symbol_name sym)) (QVariant::fromValue (s7_boolean sc val))))
      ((s7_is_real val)
       (note->setProperty (string_to_element_pid (s7_symbol_name sym)) (QVariant::fromValue (s7_real val))))
      ((s7_is_symbol val)
       (note->setProperty (string_to_element_pid (s7_symbol_name sym)) (QVariant::fromValue (string_to_ctype (s7_symbol_name val)))))
      (else
       (note->setProperty (string_to_element_pid (s7_symbol_name sym)) (QVariant::fromValue (s7_integer val)))))))

;
; emit code for ms-objects set/get, and register them to be exported to scheme
;

(def-goo-setters-sym "Ms::Breath" "breath" "symId" "setSymId" "SymId")

(def-goo-setters-bool "Ms::Note" "note" "small" "isSmall" "setSmall")
(def-goo-setters-bool "Ms::Note" "note" "ghost" "ghost" "setGhost")
(def-goo-setters-sym "Ms::Note" "note" "userDotPosition" "setUserDotPosition" "Direction")
(def-goo-setters-sym "Ms::Note" "note" "headGroup" "setHeadGroup" "NoteHead_Group")
(def-goo-setters-sym "Ms::Note" "note" "headType" "setHeadType" "NoteHead_Type")
(def-goo-setters-sym "Ms::Note" "note" "veloType" "setVeloType" "note_ValueType") ; velotype uses Note::Valuetype
(def-goo-setters-sym "Ms::Note" "note" "noteType" #f "NoteType")

(defcompile
  (defcreg ('ms-score-undoStack-undo) (2)
    (emit-pop-arg-goo '("MasterScore*" "score"))
    (emit-next-arg)
    '(raw "s = s7_car(args)")
    '(cond
      ((c_is_goo sc s)
       (raw "g = (goo_t *)s7_c_object_value(s)")
       (raw "EditData* ed = (EditData*) g->cd")
       (raw "score->undoStack()->undo(ed)"))
      (else
       (raw "score->undoStack()->undo(0)")))
    '(s7_t sc)))

; register type enumerations
(for-each (lambda (lst)
            (let ((global-list (car lst))
                  (scheme-type-name (cadr lst)))
              (register-c-type global-list)
              (emit-c-type-string-maps3 scheme-type-name)))
  `(
    (,%duration-type DurationType)
    (,%select-type   SelectType)
    (,%updown-mode UpDownMode)
    (,%key-type Key)
    (,%tpc-enum Tpc)))

; FIX: loop through %c-types and emit
(emit-c-type-string-maps3 'SegmentType)
(emit-c-type-string-maps3 'AccidentalType)

(defcompile
  (defcreg ('ms-make-fraction) (2)
    (emit-pop-arg-int '("numerator"))
    (emit-next-arg)
    (emit-pop-arg-int '("denominator"))
    '(raw "Fraction* f = new Fraction(numerator, denominator)")
    '(raw "uint64_t ty = static_cast<uint64_t>(0); // GOO_TYPE::ElementType__FRACTION")
    '(c_make_goo sc ty (s7_f sc) f)))

(defcompile
  (defcreg ('ms-division) ()
    '(s7_make_integer sc "MScore::division")))

(defcompile
  (defcreg ('ms-tpc2degree) (2)
    (emit-pop-arg-sym '("tpc"))
    (emit-next-arg)
    (emit-pop-arg-sym '("key"))
    '(s7_make_integer sc (tpc2degree (string_to_Tpc tpc) (string_to_Key key)))))

(defcompile
  (defcreg ('ms-make-ChangeStyleVal) (3)
    (emit-pop-arg-goo '("Score*" "score"))
    (emit-next-arg)
    (emit-pop-arg-sym '("sym"))
    (emit-next-arg)
    (emit-pop-arg-bool '("variant"))
    '(raw "Ms::Sid sid = string_to_Sid(sym)")
    '(raw "ChangeStyleVal* x = new ChangeStyleVal(score, sid, variant)")
    (emit-return-goo "x" "static_cast<uint64_t>(0)")))

;;;
;;; emit all declaratively stated c++object functions
;;;

(emit-registered-objects)

; emit c-functions for simple object-methods
(map (lambda (lst)
       (match lst
         ((sname cvartype cvarname meth crestype cresvarname gootype)
          (defcompile
            (defcreg (sname) (1)
               (emit-pop-arg-goo `(,cvartype ,cvarname))
               `(raw ,(format #f "~a ~a = ~a" crestype cresvarname meth))
               (emit-return-goo cresvarname (format #f "static_cast<uint64_t>(~a)" gootype)))))))
     '((ms-chords-first "QVector<Ms::Chord*>*" "chords" "chords->first()" "Ms::Chord*" "chord" "GOO_TYPE::CHORD")
       (ms-notes-front "std::vector<Note*>*" "notes" "notes->front()" "Note*" "note" "GOO_TYPE::NOTE")))

; emit simple iterators

(defcompile
  (defcreg ('ms-notes-size) (1 0)
    (emit-pop-arg-goo '("std::vector<Note*>*" "notes"))
    '(s7_make_integer sc (notes->size))))

(defcompile
  (defcreg ('ms-notes-ref) (2 0)
    (emit-pop-arg-goo '("std::vector<Note*>*" "notes"))
    (emit-next-arg)
    (emit-pop-arg-int '("i"))
    '(if (>= i (notes->size))
         (return (s7_f sc)))
    '(raw "Note* note = notes->at(i)")
    (emit-return-goo "note" "GOO_TYPE::ElementType__NOTE")))

(emit-string-to-ctype)

; emit the init-function that scheme-exports all glue-functions (ms-objects set/get)
;
(format %c "void init_gen_s7 (s7_scheme *sc) {~%")

(for-each (lambda (lst)
  (match lst
    ((objname memname memnameset)
     (if memnameset ; both get and setter is defined
       (format %c "s7_define_variable(sc, \"~a\", s7_dilambda(sc, \"~a\", ~a, 1, 0, ~a, 2, 0, \"~a\"));~%"
        (format #f "ms-~a-~a" objname memname)
        (format #f "ms-~a-~a" objname memname)
        (format #f "ms_~a_~a" objname memname)
        (format #f "ms_set_~a_~a" objname memname)
        (format #f "~a ~a field" objname memname))
       ; only getter is defined
       (format %c "s7_define_function(sc, \"~a\", ~a, 1, 0, false, \"~a\");~%"
        (format #f "ms-~a-~a" objname memname)
        (format #f "ms_~a_~a" objname memname)
        (format #f "~a ~a field" objname memname))))))
  %export-to-scheme-getset)

(emit-exports)
(emit-exports-getset)

(for-each (lambda (lst)
  (match lst
    ((scm-name c-name-get c-name-set desc)
     (format %h "s7_pointer ~a (s7_scheme *sc, s7_pointer args);~%" c-name-get)
     (format %h "s7_pointer ~a (s7_scheme *sc, s7_pointer args);~%" c-name-set)
     (format %c "s7_define_variable(sc, \"~a\", s7_dilambda(sc, \"~a\", ~a, 1, 0, ~a, 2, 0, \"~a\"));~%" scm-name scm-name c-name-get c-name-set desc))))
  '(("ms-note-usermirror" "ms_note_usermirror" "ms_set_note_usermirror" "note usermirror field")
    ("ms-tremolo-tremoloType" "ms_tremolo_tremoloType" "ms_set_tremolo_tremoloType" "tremolo type file")
    ("ms-element-parent" "ms_element_parent" "ms_element_setParent" "element parent field")))
(format %c "}~%")

(gen-done)
