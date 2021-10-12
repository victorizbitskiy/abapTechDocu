*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sb_input WITH FRAME TITLE text-001.

SELECT-OPTIONS s_treqs FOR e071-trkorr NO INTERVALS MODIF ID rnm OBLIGATORY.
PARAMETERS p_lang TYPE spras OBLIGATORY.

SELECTION-SCREEN END OF BLOCK sb_input.
*
*SELECTION-SCREEN BEGIN OF BLOCK sb_output WITH FRAME TITLE sb_name2.
*PARAMETERS p_path TYPE string LOWER CASE OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK sb_output.
