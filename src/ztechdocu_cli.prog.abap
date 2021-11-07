*&---------------------------------------------------------------------*
*& Include          ZTECHDOCU_CLI
*&---------------------------------------------------------------------*
CLASS lcl_techdocu_scr_events IMPLEMENTATION.
  METHOD initialization.
    p_lang = sy-langu.
  ENDMETHOD.

  METHOD at_selection_screen.

    IF s_treqs IS INITIAL AND s_devc[] IS INITIAL.
      MESSAGE e001.
    ENDIF.

  ENDMETHOD.

  METHOD at_ssonvrf_treqs.

    DATA ls_selection TYPE trwbo_selection.
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

    go_repo = NEW lcl_techdocu_repo( VALUE #( treq_range = s_treqs[]
                                              devc_range = s_devc[]
                                              lang = p_lang ) ).
    go_repo->read( ).

  ENDMETHOD.

  METHOD end_of_selection.

    go_repo->display( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_repo IMPLEMENTATION.
  METHOD constructor.

    ms_context = is_context.

  ENDMETHOD.

  METHOD select_treqs.

    DATA lt_requests TYPE trwbo_request_headers.
    DATA ls_ranges TYPE trsel_ts_ranges.

    IF ms_context-treq_range IS INITIAL.
      RETURN.
    ENDIF.

    ls_ranges-trkorr = ms_context-treq_range.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        iv_complete_projects   = abap_true
      IMPORTING
        et_requests            = lt_requests
      CHANGING
        cs_ranges              = ls_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc = 0.
      SORT lt_requests BY trkorr.
      DELETE ADJACENT DUPLICATES FROM lt_requests COMPARING trkorr.
    ENDIF.

    LOOP AT lt_requests ASSIGNING FIELD-SYMBOL(<ls_requests>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_requests>-trkorr ) TO rt_result.
    ENDLOOP.

  ENDMETHOD.

  METHOD read_repo_data.

    DATA lt_repo_data TYPE ty_t_repo_data.
    DATA lt_object_table TYPE STANDARD TABLE OF ko100.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_object_table.

    APPEND LINES OF read_e071( ) TO lt_repo_data.
    APPEND LINES OF read_tadir( ) TO lt_repo_data.

    SORT lt_repo_data ASCENDING.

    LOOP AT lt_repo_data INTO DATA(ls_repo_data).

      IF is_treq_exist( ls_repo_data ).
        ls_repo_data-obj_type_name = lt_object_table[ object = ls_repo_data-obj_type ]-text.

        TRY.
            ls_repo_data-obj_title = read_repo_object_title( iv_object = ls_repo_data-obj_name
                                                             iv_object_type = ls_repo_data-obj_type ).

          CATCH cx_sy_create_object_error INTO DATA(lo_e).

            ls_repo_data-rowcolor = 'C311'.
            ls_repo_data-message_type = 'W'.
            MESSAGE w002 WITH ls_repo_data-obj_type INTO ls_repo_data-message_text.

        ENDTRY.

        IF ls_repo_data-message_type IS INITIAL.
          APPEND ls_repo_data TO mt_repo_data.
        ELSE.
          INSERT ls_repo_data INTO mt_repo_data INDEX 1.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_e071.

    DATA(lt_treqs) = select_treqs( ).

    IF lt_treqs IS NOT INITIAL.

      SELECT DISTINCT
        pgmid,
        object AS obj_type,
        obj_name
        FROM e071
        INTO CORRESPONDING FIELDS OF TABLE @rt_result
        WHERE trkorr IN @lt_treqs
          AND pgmid = 'R3TR'.

    ENDIF.

  ENDMETHOD.

  METHOD read_tadir.

    IF ms_context-devc_range IS NOT INITIAL.

      SELECT
        pgmid,
        object AS obj_type,
        obj_name
        FROM tadir
        APPENDING CORRESPONDING FIELDS OF TABLE @rt_result
        WHERE devclass IN @ms_context-devc_range
          AND pgmid = 'R3TR'.

    ENDIF.

  ENDMETHOD.

  METHOD is_treq_exist.

    DATA lv_exist TYPE boole_d.

    CALL FUNCTION 'TR_CHECK_EXIST'
      EXPORTING
        iv_pgmid             = is_repo_data-pgmid
        iv_object            = is_repo_data-obj_type
        iv_obj_name          = is_repo_data-obj_name
      IMPORTING
        e_exist              = lv_exist
      EXCEPTIONS
        tr_no_check_function = 1
        OTHERS               = 2.
    IF sy-subrc = 0 AND lv_exist = abap_true.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD read_repo_object_title.

    DATA(lo_object) = lcl_techdocu_repo_object=>get_instance( iv_object = iv_object
                                                              iv_object_type = iv_object_type
                                                              iv_lang = ms_context-lang ).
    rv_result = lo_object->title( ).

  ENDMETHOD.

  METHOD lif_techdocu_repo~read.

    read_repo_data( ).
    ro_result = me.

  ENDMETHOD.

  METHOD lif_techdocu_repo~display.

    DATA(lo_alv) = NEW lcl_techdocu_alv( ).
    lo_alv->display( CHANGING ct_outtab = mt_repo_data ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_alv IMPLEMENTATION.

  METHOD display.

    create_grid( CHANGING ct_outtab = ct_outtab ).
    CALL SCREEN 9001.

  ENDMETHOD.

  METHOD create_grid.

    DATA lo_container TYPE REF TO cl_gui_custom_container.
    DATA lt_fcat      TYPE lvc_t_fcat.
    DATA ls_layout    TYPE lvc_s_layo.

    lo_container = NEW #( container_name = 'CONTAINER_9001' ).
    mo_grid = NEW #( i_parent = lo_container ).

    lt_fcat = get_field_catalog( ).
    ls_layout-sel_mode = 'A'.
    ls_layout-zebra = abap_true.
    ls_layout-cwidth_opt = abap_true.
    ls_layout-info_fname = 'ROWCOLOR'.

    mo_grid->set_table_for_first_display( EXPORTING is_layout       = ls_layout
                                           CHANGING it_fieldcatalog = lt_fcat
                                                    it_outtab       = ct_outtab ).
  ENDMETHOD.

  METHOD get_field_catalog.

    FIELD-SYMBOLS <ls_fcat> LIKE LINE OF rt_fcat.

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
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Program ID'(002).
          WHEN 'OBJ_TYPE'.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Object type'(003).
          WHEN 'OBJ_TYPE_NAME'.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Object type name'(004).
          WHEN 'OBJ_NAME'.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Object name'(005).
          WHEN 'OBJ_TITLE'.
            <ls_fcat>-outputlen = 12.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Object title'(006).
          WHEN 'MESSAGE_TYPE'.
            <ls_fcat>-outputlen = 12.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Message type'(007).
          WHEN 'MESSAGE_TEXT'.
            <ls_fcat>-outputlen = 20.
            <ls_fcat>-scrtext_s =
            <ls_fcat>-scrtext_m =
            <ls_fcat>-scrtext_l =
            <ls_fcat>-coltext = 'Message text'(008).
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object IMPLEMENTATION.
  METHOD constructor.

    mv_object = iv_object.
    mv_object_type = iv_object_type.
    mv_lang = iv_lang.

  ENDMETHOD.

  METHOD get_instance.

    DATA(lv_type) = |LCL_TECHDOCU_REPO_OBJECT_{ iv_object_type }|.

    CREATE OBJECT ro_result
      TYPE (lv_type)
      EXPORTING
        iv_object = iv_object
        iv_object_type = iv_object_type
        iv_lang = iv_lang.

  ENDMETHOD.

  METHOD lif_techdocu_repo_object~title.
    rv_result = space.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_techdocu_repo_object_devc IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ctext FROM tdevct INTO @rv_result WHERE devclass = @mv_object
                                                      AND spras = @mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_prog IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    DATA lv_result TYPE rs38m-repti.

    CALL FUNCTION 'PROGRAM_TITLE'
      EXPORTING
        program  = CONV sy-repid( mv_object )
        language = mv_lang
      IMPORTING
        title    = lv_result.

    rv_result = lv_result.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_intf IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    DATA ls_clskey TYPE seoclskey.
    DATA ls_vseointerf TYPE vseointerf.

    ls_clskey-clsname = mv_object.

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
      rv_result = ls_vseointerf-descript.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_clas IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    DATA ls_clskey TYPE seoclskey.
    DATA ls_vseoclass TYPE vseoclass.

    ls_clskey-clsname = mv_object.

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
      rv_result = ls_vseoclass-descript.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_tabl IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd02v INTO @rv_result WHERE tabname = @mv_object
                                                      AND ddlanguage = @mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_msag IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE stext FROM t100a INTO @rv_result WHERE arbgb = @mv_object
                                                     AND masterlang = @mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_shlp IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd30t INTO @rv_result WHERE shlpname = @mv_object
                                                      AND ddlanguage = @mv_lang
                                                      AND as4local = 'A'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_doma IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd01t INTO @rv_result WHERE domname = @mv_object
                                                      AND ddlanguage = @mv_lang
                                                      AND as4local = 'A'
                                                      AND as4vers = @space.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_dtel IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd04t INTO @rv_result WHERE rollname = @mv_object
                                                      AND ddlanguage = @mv_lang
                                                      AND as4local = 'A'
                                                      AND as4vers = @space.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_ttyp IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd40t INTO @rv_result WHERE typename = @mv_object
                                                      AND ddlanguage = @mv_lang
                                                      AND as4local = 'A'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_view IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE ddtext FROM dd25t INTO @rv_result WHERE ddlanguage = @mv_lang
                                                      AND viewname = @mv_object
                                                      AND as4local = 'A'
                                                      AND as4vers = @space.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_sfpi IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE text INTO @rv_result FROM fpinterfacet WHERE name = @mv_object
                                                           AND state = 'A'
                                                           AND language = @mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_sfpf IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE text INTO @rv_result FROM fpcontextt WHERE name = @mv_object
                                                         AND state = 'A'
                                                         AND language = @mv_lang.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_fugr IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE areat INTO @rv_result FROM tlibt WHERE spras = @mv_lang
                                                     AND area = @mv_object.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_techdocu_repo_object_sxci IMPLEMENTATION.
  METHOD lif_techdocu_repo_object~title.

    SELECT SINGLE text INTO @rv_result FROM sxc_attrt WHERE imp_name = @mv_object
                                                        AND sprsl = @mv_lang.
  ENDMETHOD.
ENDCLASS.
