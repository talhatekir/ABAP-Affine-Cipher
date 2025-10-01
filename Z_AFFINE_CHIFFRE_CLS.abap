*&---------------------------------------------------------------------*
*& Include          Z_AFFINE_CHIFFRE_CLS
*&---------------------------------------------------------------------*
CLASS zcl_affine DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: c_m      TYPE i VALUE 26,
               c_alpha  TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
               c_digits TYPE string VALUE '0123456789'.

    CLASS-METHODS:
      encrypt IMPORTING  iv_a   TYPE i
                         iv_b   TYPE i
                         iv_txt TYPE string
              EXPORTING  ev_txt TYPE string
              EXCEPTIONS invalid_text
                         invalid_a,

      decrypt IMPORTING  iv_a   TYPE i
                         iv_b   TYPE i
                         iv_txt TYPE string
              EXPORTING  ev_txt TYPE string
              EXCEPTIONS invalid_text
                         invalid_a,

      is_coprime IMPORTING iv_a      TYPE i
                           iv_b      TYPE i
                 RETURNING VALUE(rv) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-METHODS modinv       IMPORTING iv_a  TYPE i iv_m TYPE i RETURNING VALUE(rv) TYPE i.
    CLASS-METHODS pos_in_alpha IMPORTING iv_ch TYPE c RETURNING VALUE(rv) TYPE i.

ENDCLASS.

CLASS zcl_affine IMPLEMENTATION.
  "--- Verschlüsselung (Affine Chiffre)
  "    Formel: E(x) = (a * i + b) mod m, mit m = 26 und i E {0..25}.
  "    Hinweis: Ziffern (0–9) bleiben unverändert; nur A–Z werden abgebildet.
  METHOD encrypt.
    "--- Eingabeprüfung: keine Leer- oder Satzzeichen erlaubt --------------------
    DATA lv_match TYPE i.
    FIND REGEX '[[:space:][:punct:]]' IN iv_txt MATCH COUNT lv_match.
    IF lv_match > 0.
      RAISE invalid_text.   " ungültiges Zeichen im Eingabetext
      EXIT.
    ENDIF.

    "--- Schlüsselprüfung: a und 26 müssen teilerfremd (koprim) sein ------------
    IF zcl_affine=>is_coprime( iv_a = iv_a iv_b = c_m ) EQ abap_false.
      RAISE invalid_a.
      EXIT.
    ENDIF.

    "--- Vorbereitung: auf Großbuchstaben normalisieren, Ergebnis leeren --------
    DATA(lv_text) = iv_txt.
    TRANSLATE lv_text TO UPPER CASE.
    CLEAR ev_txt.

    "--- Laufvariablen (nur für die Schleife) -----------------------------------
    DATA lv_ch TYPE c LENGTH 1.         " aktuelles Zeichen
    DATA lv_i TYPE i.                   " Alphabetindex (A->0 ... Z->25)
    DATA lv_y TYPE i.                   " verschlüsselter Index
    DATA lv_idx TYPE i.                 " 0-basierter Offset im String

    "--- Hauptschleife: Zeichen für Zeichen verarbeiten -------------------------
    DO strlen( lv_text ) TIMES.
      lv_idx = sy-index - 1.            " DO zählt ab 1; Offset ist 0-basiert
      lv_ch  = lv_text+lv_idx(1).       " genau 1 Zeichen holen

      " Ziffern werden nicht verschlüsselt -> unverändert übernehmen
      IF lv_ch CA c_digits.
        ev_txt = ev_txt && lv_ch.
        CONTINUE.
      ENDIF.

      " Nur A–Z verschlüsseln (alles andere wurde zuvor per Regex verboten)
      lv_i = zcl_affine=>pos_in_alpha( lv_ch ).
      IF lv_i >= 0.
        " Affine Verschlüsselung: E(x) = (a*i + b) mod m
        lv_y  = ( iv_a * lv_i + iv_b ) MOD c_m.
        ev_txt = ev_txt && c_alpha+lv_y(1).
      ENDIF.
      " (Defensivfall: falls lv_i < 0, wird das Zeichen übersprungen)
    ENDDO.
  ENDMETHOD.


  "--- Entschlüsselung (Affine Chiffre)
  "    Formel: D(y) = a⁻¹ * (y - b) mod m, mit m = 26.
  "    a⁻¹ ist die modulare Inverse von a (mod 26); Ziffern bleiben unverändert.
  METHOD decrypt.
    "--- Eingabeprüfung: keine Leer- oder Satzzeichen erlaubt --------------------
    DATA lv_match TYPE i.
    FIND REGEX '[[:space:][:punct:]]' IN iv_txt MATCH COUNT lv_match.
    IF lv_match > 0.
      RAISE invalid_text.
      EXIT.
    ENDIF.

    "--- Schlüsselprüfung: a und 26 müssen teilerfremd (koprim) sein -------------
    IF zcl_affine=>is_coprime( iv_a = iv_a iv_b = c_m ) EQ abap_false.
      RAISE invalid_a.
      EXIT.
    ENDIF.

    "--- Modulare Inverse berechnen: a⁻¹ (mod 26) --------------------------------
    DATA(lv_ainv) = zcl_affine=>modinv( iv_a = iv_a iv_m = c_m ).

    "--- Vorbereitung: auf Großbuchstaben normalisieren, Ergebnis leeren --------
    DATA(lv_text) = iv_txt.
    TRANSLATE lv_text TO UPPER CASE.
    CLEAR ev_txt.

    "--- Laufvariablen ----------------------------------------------------------
    DATA: lv_idx TYPE i,                 " 0-basierter Offset im String
          lv_ch  TYPE c LENGTH 1,        " aktuelles Zeichen
          lv_y   TYPE i,                 " Index im Alphabet (Chiffre)
          lv_x   TYPE i.                 " zurückgerechneter Index (Klartext)

    "--- Hauptschleife: Zeichen für Zeichen verarbeiten -------------------------
    DO strlen( lv_text ) TIMES.
      lv_idx = sy-index - 1.             " DO: 1..N  -> Offset: 0..N-1
      lv_ch  = lv_text+lv_idx(1).        " ein Zeichen holen

      " Ziffern werden nicht entschlüsselt -> unverändert übernehmen
      IF lv_ch CA c_digits.
        ev_txt = ev_txt && lv_ch.
        CONTINUE.
      ENDIF.

      " Nur A–Z verarbeiten (alles andere war schon per Regex verboten)
      lv_y = zcl_affine=>pos_in_alpha( lv_ch ).
      IF lv_y >= 0.
        " Entschlüsselung: D(y) = a⁻¹ * (y - b) mod m
        lv_x  = ( lv_ainv * ( lv_y - iv_b ) ) MOD c_m.
        ev_txt = ev_txt && c_alpha+lv_x(1).
      ENDIF.
    ENDDO.
  ENDMETHOD.


  "--- Prüft Teilerfremdheit von iv_a und iv_b mittels euklidischem Algorithmus
  "   Rückgabe: abap_true, wenn gcd(iv_a, iv_b) = 1; sonst abap_false
  METHOD is_coprime.
    " Arbeitsvariablen für gcd
    DATA: a TYPE i,      " laufender Dividend
          b TYPE i,      " laufender Divisor
          t TYPE i.      " temporärer Rest

    a = iv_a.
    b = iv_b.

    " Klassischer Euklid: wiederhole, bis der Rest 0 ist
    WHILE b <> 0.
      t = a MOD b.
      a = b.
      b = t.
    ENDWHILE.

    " a enthält nun gcd(iv_a, iv_b) (Vorzeichen egal)
    rv = COND abap_bool( WHEN a = 1 OR a = -1 THEN abap_true ELSE abap_false ).
  ENDMETHOD.


  "--- Liefert die Position des Zeichens im Alphabet A–Z
  "   'A'→0 … 'Z'→25; sonst -1 (kein Buchstabe im verwendeten Alphabet)
  "   Hinweis: basiert auf der Konstanten c_alpha = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  METHOD pos_in_alpha.
    DATA lv_off TYPE i.  " gefundener Offset im Alphabet

    FIND iv_ch IN c_alpha MATCH OFFSET lv_off.
    rv = COND i( WHEN sy-subrc = 0 THEN lv_off ELSE -1 ).
  ENDMETHOD.


  "--- Modulare Inverse: berechnet a⁻¹ (mod m) mit erweitertem Euklid
  "   Voraussetzung: a und m sind teilerfremd (sonst existiert keine Inverse)
  "   Ergebnis rv erfüllt: (iv_a * rv) MOD iv_m = 1
  "   Hinweis: Hier wird nur der s-Anteil (Koeffizient vor a) verfolgt
  METHOD modinv.
    " old_r / r: Restfolgen (gcd-Läufe)
    " old_s / s: Koeffizienten so, dass old_s*iv_a + ?*iv_m = old_r
    DATA: old_r TYPE i,        " startet mit a
          r     TYPE i,        " startet mit m
          old_s TYPE i VALUE 1, " s-Koeff. zu a (initial 1)
          s     TYPE i VALUE 0, " s-Koeff. zu a (initial 0)
          q     TYPE i,        " Quotient der Division
          tmp   TYPE i.        " Tausch-Temp

    old_r = iv_a.
    r     = iv_m.

    " Erweiterter Euklid: Reste und s-Koeffizienten iterativ aktualisieren
    WHILE r <> 0.
      q = old_r DIV r.

      " Rest-Update
      tmp   = r.
      r     = old_r - q * r.
      old_r = tmp.

      " s-Update (nur a-Koeffizient ist für die Inverse relevant)
      tmp   = s.
      s     = old_s - q * s.
      old_s = tmp.
    ENDWHILE.

    " Am Ende gilt old_r = gcd(iv_a, iv_m) = 1 (durch Vorprüfung sichergestellt)
    " old_s ist die gesuchte Inverse (evtl. negativ) → auf [0..m-1] bringen
    rv = old_s MOD iv_m.
  ENDMETHOD.

ENDCLASS.
