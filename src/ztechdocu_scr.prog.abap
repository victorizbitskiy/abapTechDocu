*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sb_input WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS s_treqs FOR e071-trkorr NO INTERVALS.
SELECT-OPTIONS s_devc FOR tadir-devclass NO INTERVALS.
PARAMETERS p_lang TYPE spras OBLIGATORY.

SELECTION-SCREEN END OF BLOCK sb_input.
