*&---------------------------------------------------------------------*
*& Include          Z_AFFINE_CHIFFRE_TOP
*&---------------------------------------------------------------------*
DATA gv_result TYPE string.
DATA(invalid_text) = |Leerzeichen und Satzzeichen sind nicht erlaubt im Eingabetext|.
DATA(invalid_a) =
|Die Werte Verschlüsselungsschlüssel A und 26(die Länge des Alphabets) müssen koprim (relativ prim) sein|.
