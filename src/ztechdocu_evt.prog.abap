*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_EVT
*&---------------------------------------------------------------------*
INITIALIZATION.
  lcl_techdocu_scr_events=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_treqs-low.
  lcl_techdocu_scr_events=>at_ssonvrf_treqs( ).

START-OF-SELECTION.
  lcl_techdocu_scr_events=>start_of_selection( ).

END-OF-SELECTION.
  lcl_techdocu_scr_events=>end_of_selection( ).
