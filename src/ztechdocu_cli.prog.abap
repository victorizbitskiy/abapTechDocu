*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_CLI
*&---------------------------------------------------------------------*
CLASS lcl_techdocu_scr_events IMPLEMENTATION.
  METHOD initialization.
    p_lang = 'EN'.
  ENDMETHOD.

  METHOD at_ssonvrf_treqs.

    DATA ls_selection        TYPE trwbo_selection.
    DATA ls_selected_request TYPE trwbo_request_header.

    CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
      EXPORTING
        iv_username         = '*'
        iv_organizer_type   = 'T'
        is_selection        = ls_selection
      IMPORTING
        es_selected_request = ls_selected_request.

    s_treqs-low = ls_selected_request-trkorr.

  ENDMETHOD.

  METHOD start_of_selection.

    go_treq_objects = NEW lcl_techdocu_treq_objects( it_treqs = s_treqs[]
                                                     iv_lang = p_lang ).
    go_treq_objects->read( ).

  ENDMETHOD.

  METHOD end_of_selection.

    go_treq_objects->display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_treq_objects IMPLEMENTATION.
  METHOD constructor.

    mt_treqs = it_treqs.
    mv_lang = iv_lang.

  ENDMETHOD.

  METHOD select_treqs.

    DATA lt_requests TYPE  trwbo_request_headers.
    DATA cs_ranges TYPE trsel_ts_ranges.

    cs_ranges-trkorr = mt_treqs.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        iv_complete_projects   = abap_true
      IMPORTING
        et_requests            = lt_requests
      CHANGING
        cs_ranges              = cs_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      SORT lt_requests BY trkorr.
      DELETE ADJACENT DUPLICATES FROM lt_requests COMPARING trkorr.
    ENDIF.

    LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<lfs_requests>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <lfs_requests>-trkorr ) TO rt_result.
    ENDLOOP.

  ENDMETHOD.

  METHOD read_treqs_data.

    DATA lt_treqs_data TYPE ty_t_treqs_data.
    DATA lt_object_table TYPE STANDARD TABLE OF ko100.

    SELECT DISTINCT
      object AS obj_type,
      pgmid,
      obj_name
      FROM e071
      INTO CORRESPONDING FIELDS OF TABLE @lt_treqs_data
      WHERE trkorr IN @it_treqs
        AND pgmid = 'R3TR'
      ORDER BY obj_type.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_object_table.

    LOOP AT lt_treqs_data INTO DATA(ls_treqs_data).

      IF is_treq_exist( ls_treqs_data ).
        ls_treqs_data-obj_type_name = lt_object_table[ object = ls_treqs_data-obj_type ]-text.

        ls_treqs_data-obj_title = read_treq_object_title( iv_treq_object      = ls_treqs_data-obj_name
                                                          iv_treq_object_type = ls_treqs_data-obj_type ).
        APPEND ls_treqs_data TO mt_treqs_data.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD is_treq_exist.

    DATA lv_exist TYPE boole_d.

    CALL FUNCTION 'TR_CHECK_EXIST'
      EXPORTING
        iv_pgmid             = is_treqs_data-pgmid
        iv_object            = is_treqs_data-obj_type
        iv_obj_name          = is_treqs_data-obj_name
      IMPORTING
        e_exist              = lv_exist
      EXCEPTIONS
        tr_no_check_function = 1
        OTHERS               = 2.
    IF sy-subrc = 0 AND lv_exist = abap_true.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD read_treq_object_title.

    TRY.
        DATA(lo_treq_object) =
          lcl_techdocu_treq_object=>get_instance( iv_treq_object      = iv_treq_object
                                                  iv_treq_object_type = iv_treq_object_type
                                                  iv_lang             = mv_lang ).
        rv_result = lo_treq_object->title( ).
      CATCH cx_sy_create_object_error INTO DATA(lo_e).
        "TODO Raise Exception
        MESSAGE lo_e->get_text( ) TYPE 'S' DISPLAY LIKE 'W'.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_techdocu_treq_objects~read.

    DATA(lt_treqs) = select_treqs( ).
    read_treqs_data( lt_treqs ).
    ro_result = me.

  ENDMETHOD.

  METHOD lif_techdocu_treq_objects~display.

    DATA(lo_alv) = NEW lcl_techdocu_alv( mt_treqs_data ).
    lo_alv->display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_alv IMPLEMENTATION.
  METHOD constructor.
    mt_grid = it_grid.
  ENDMETHOD.

  METHOD display.
    create_grid( ).
    CALL SCREEN 9001.
  ENDMETHOD.

  METHOD create_grid.
    DATA: lo_container TYPE REF TO cl_gui_custom_container,
          lt_fcat      TYPE lvc_t_fcat,
          ls_layout    TYPE lvc_s_layo.

    CREATE OBJECT lo_container
      EXPORTING
        container_name = 'CONTAINER_9001'.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent = lo_container.

    lt_fcat = get_field_catalog( ).
    ls_layout-sel_mode = 'A'.
    ls_layout-zebra = abap_true.
    ls_layout-cwidth_opt = abap_true.

    mo_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout
                                           CHANGING it_fieldcatalog = lt_fcat
                                                    it_outtab       = mt_grid ).
  ENDMETHOD.

  METHOD get_field_catalog.

    FIELD-SYMBOLS: <ls_fcat> LIKE LINE OF rt_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZTECHDOCU_DATA'
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      LOOP AT rt_fcat ASSIGNING <ls_fcat>.

        CASE <ls_fcat>-fieldname.
          WHEN 'PGMID'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = 'Program ID'(002).
          WHEN 'OBJ_TYPE'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = 'Object type'(003).
          WHEN 'OBJ_TYPE_NAME'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = 'Object type name'(004).
          WHEN 'OBJ_NAME'.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = 'Object name'(005).
          WHEN 'OBJ_TITLE'.
            <ls_fcat>-outputlen = 12.
            <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = 'Object Title'(006).
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object IMPLEMENTATION.
  METHOD constructor.

    mv_treq_object = iv_treq_object.
    mv_treq_object_type = iv_treq_object_type.
    mv_lang = iv_lang.

  ENDMETHOD.

  METHOD get_instance.

    DATA(lv_type) = |LCL_TECHDOCU_TREQ_OBJECT_{ iv_treq_object_type }|.

    CREATE OBJECT ro_result
      TYPE (lv_type)
      EXPORTING
        iv_treq_object = iv_treq_object
        iv_treq_object_type = iv_treq_object_type
        iv_lang = iv_lang.

  ENDMETHOD.

  METHOD lif_techdocu_treq_object~title.
    rv_result = space.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_treq_object_devc IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    SELECT SINGLE ctext FROM tdevct INTO rv_result WHERE devclass = mv_treq_object
                                                     AND spras = mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_prog IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    DATA lv_result TYPE rs38m-repti.

    CALL FUNCTION 'PROGRAM_TITLE'
      EXPORTING
        program  = CONV sy-repid( mv_treq_object )
        language = mv_lang
      IMPORTING
        title    = lv_result.

    rv_result = lv_result.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_intf IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    DATA ls_clskey TYPE seoclskey.
    DATA ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = mv_treq_object.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        interface    = ls_vseointerf
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 0.
      rv_result = ls_vseointerf-clsname.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_clas IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    DATA ls_clskey TYPE seoclskey.
    DATA ls_vseoclass TYPE vseoclass.

    ls_clskey-clsname = mv_treq_object.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 0.
      rv_result = ls_vseoclass-clsname.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_tabl IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    SELECT SINGLE ddtext FROM dd02v INTO rv_result WHERE tabname = mv_treq_object
                                                     AND ddlanguage = mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_msag IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    SELECT SINGLE stext FROM t100a INTO rv_result WHERE arbgb = mv_treq_object
                                                    AND masterlang = mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_treq_object_shlp IMPLEMENTATION.
  METHOD lif_techdocu_treq_object~title.

    SELECT SINGLE ddtext FROM dd30t INTO rv_result WHERE shlpname = mv_treq_object
                                                     AND ddlanguage = mv_lang
                                                     AND as4local = 'A'.
  ENDMETHOD.
ENDCLASS.
