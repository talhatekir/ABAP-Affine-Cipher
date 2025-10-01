*&---------------------------------------------------------------------*
*& Report Z_AFFINE_CHIFFRE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_affine_chiffre.

INCLUDE z_affine_chiffre_top.
INCLUDE z_affine_chiffre_scr.
INCLUDE z_affine_chiffre_cls.
INCLUDE z_affine_chiffre_frm.

AT SELECTION-SCREEN OUTPUT.
  CLEAR: p_a, p_b, p_txt.

START-OF-SELECTION.
  IF p_enc EQ 'X'.
    PERFORM encrypt_text.
  ELSEIF p_dec EQ 'X'.
    PERFORM decrypt_text.
  ENDIF.
