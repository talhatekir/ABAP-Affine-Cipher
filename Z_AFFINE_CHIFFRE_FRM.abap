*&---------------------------------------------------------------------*
*& Include          Z_AFFINE_CHIFFRE_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ENCRYPT_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encrypt_text .
  zcl_affine=>encrypt(
      EXPORTING
        iv_a         = p_a
        iv_b         = p_b
        iv_txt       = p_txt
      IMPORTING
        ev_txt       = DATA(gv_result2)
      EXCEPTIONS
        invalid_text = 1
        invalid_a    = 2
        OTHERS       = 3
    ).
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE invalid_text TYPE 'S' DISPLAY LIKE 'E'.
      WHEN 2.
        MESSAGE invalid_a TYPE 'S' DISPLAY LIKE 'E'.
      WHEN OTHERS.
        MESSAGE |Versuch nochmal| TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.
  ELSE.
    WRITE gv_result2.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DECRYPT_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM decrypt_text .
  zcl_affine=>decrypt(
  EXPORTING
    iv_a         = p_a
    iv_b         = p_b
    iv_txt       = p_txt
  IMPORTING
    ev_txt       = gv_result
  EXCEPTIONS
    invalid_text = 1
    invalid_a    = 2
    OTHERS       = 3
).
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE invalid_text TYPE 'S' DISPLAY LIKE 'E'.
      WHEN 2.
        MESSAGE invalid_a TYPE 'S' DISPLAY LIKE 'E'.
      WHEN OTHERS.
        MESSAGE |Versuch nochmal| TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.
  ELSE.
    WRITE gv_result.
  ENDIF.
ENDFORM.
