*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_MDL
*&---------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS '9001'.
  SET TITLEBAR '9001'.
ENDMODULE.

MODULE user_command_9001 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
