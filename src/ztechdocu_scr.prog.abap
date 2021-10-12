*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sb_input WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS s_treqs FOR e071-trkorr NO INTERVALS MODIF ID rnm OBLIGATORY.
PARAMETERS p_lang TYPE spras OBLIGATORY.

SELECTION-SCREEN END OF BLOCK sb_input.
