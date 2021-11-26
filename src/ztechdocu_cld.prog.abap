*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_CLD
*&---------------------------------------------------------------------*
INTERFACE lif_techdocu_repo.
  METHODS:
    read RETURNING VALUE(ro_result) TYPE REF TO lif_techdocu_repo,

    display.

ENDINTERFACE.

CLASS lcl_techdocu_repo_obj_metadata DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_attributes,
        title TYPE string,
        cnam  TYPE string,
        cdat  TYPE d,
        unam  TYPE string,
        udat  TYPE d,
      END OF ty_attributes.

    METHODS:
      set_attributes IMPORTING is_attributes TYPE ty_attributes,
      get_attributes RETURNING VALUE(rs_result) TYPE ty_attributes.

  PRIVATE SECTION.
    DATA ms_attributes TYPE ty_attributes.

ENDCLASS.

CLASS lcl_techdocu_repo DEFINITION FINAL.
  PUBLIC SECTION.

    INTERFACES lif_techdocu_repo.

    ALIASES read FOR lif_techdocu_repo~read.
    ALIASES display FOR lif_techdocu_repo~display.

    TYPES ty_t_treq TYPE RANGE OF trkorr.
    TYPES ty_t_devc TYPE RANGE OF devclass.

    TYPES:
      BEGIN OF ty_context,
        treq_range TYPE ty_t_treq,
        devc_range TYPE ty_t_devc,
        lang       TYPE sy-langu,
      END OF ty_context.

    TYPES ty_repo_data TYPE ztechdocu_data.
    TYPES ty_t_repo_data TYPE STANDARD TABLE OF ty_repo_data WITH KEY primary_key COMPONENTS obj_type.

    METHODS constructor IMPORTING is_context TYPE ty_context.

  PRIVATE SECTION.
    DATA ms_context TYPE ty_context.
    DATA mt_repo_data TYPE ty_t_repo_data.

    METHODS:
      select_treqs RETURNING VALUE(rt_result) TYPE ty_t_treq,

      read_repo_data,

      repo_data_by_treqs RETURNING VALUE(rt_result) TYPE ty_t_repo_data,

      repo_data_by_package RETURNING VALUE(rt_result) TYPE ty_t_repo_data,

      is_tr_obj_exist IMPORTING is_repo_data     TYPE ty_repo_data
                      RETURNING VALUE(rv_result) TYPE boole_d.

ENDCLASS.

CLASS lcl_techdocu_alv DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES ty_t_outtab TYPE lcl_techdocu_repo=>ty_t_repo_data.

    METHODS display CHANGING ct_outtab TYPE ty_t_outtab.

  PRIVATE SECTION.
    DATA mo_grid TYPE REF TO cl_gui_alv_grid.

    METHODS:
      create_grid CHANGING ct_outtab TYPE ty_t_outtab,

      get_field_catalog RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.

ENDCLASS.

CLASS lcl_techdocu_scr_events DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-DATA go_repo TYPE REF TO lif_techdocu_repo.

    CLASS-METHODS:
      initialization,

      at_selection_screen,

      at_ssonvrf_treqs,

      start_of_selection,

      end_of_selection.

ENDCLASS.

INTERFACE lif_techdocu_repo_obj.
  METHODS:
    read_metadata RETURNING VALUE(ro_result) TYPE REF TO lif_techdocu_repo_obj,

    get_metadata RETURNING VALUE(ro_result) TYPE REF TO lcl_techdocu_repo_obj_metadata
                 RAISING   cx_sy_create_object_error.
ENDINTERFACE.

CLASS lcl_techdocu_repo_obj DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_techdocu_repo_obj.

    CLASS-METHODS:
      get_instance IMPORTING iv_object        TYPE trobj_name
                             iv_object_type   TYPE trobjtype
                             iv_lang          TYPE sy-langu
                   RETURNING VALUE(ro_result) TYPE REF TO lif_techdocu_repo_obj
                   RAISING   cx_sy_create_object_error.

    METHODS:
      constructor IMPORTING iv_object      TYPE trobj_name
                            iv_object_type TYPE trobjtype
                            iv_lang        TYPE sy-langu.
  PROTECTED SECTION.
    DATA mv_object      TYPE trobj_name.
    DATA mv_object_type TYPE trobjtype.
    DATA mv_lang        TYPE sy-langu.
    DATA mo_metadata TYPE REF TO lcl_techdocu_repo_obj_metadata.

ENDCLASS.

CLASS lcl_techdocu_repo_obj_devc DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      properties RETURNING VALUE(rs_result) TYPE tdevc.

ENDCLASS.

CLASS lcl_techdocu_repo_obj_prog DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS:
      lif_techdocu_repo_obj~read_metadata REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      title RETURNING VALUE(rv_result) TYPE string,

      read_trdir RETURNING VALUE(rs_result) TYPE trdir.

ENDCLASS.

CLASS lcl_techdocu_repo_obj_tran DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_intf DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      properties RETURNING VALUE(rs_result) TYPE vseointerf.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_clas DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      properties RETURNING VALUE(rs_result) TYPE vseoclass.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_tabl DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_msag DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_shlp DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_doma DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_dtel DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_ttyp DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_view DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_sfpi DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_sfpf DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_fugr DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.

CLASS lcl_techdocu_repo_obj_sxci DEFINITION INHERITING FROM lcl_techdocu_repo_obj FINAL.
  PUBLIC SECTION.
    METHODS lif_techdocu_repo_obj~read_metadata REDEFINITION.
ENDCLASS.
