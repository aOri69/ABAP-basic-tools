class ZCL_BA_LOGGER definition
  public
  create private .

public section.
  type-pools ABAP .

  constants:
    BEGIN OF MC_MSGTY,
    s type SYMSGTY VALUE 'S',
    e TYPE SYMSGTY VALUE 'E',
    w TYPE SYMSGTY VALUE 'W',
    i TYPE SYMSGTY VALUE 'I',
    a TYPE SYMSGTY VALUE 'A',
    END OF mc_msgty .
  data MS_DSP_PROF_GLOBAL type BAL_S_PROF .
  data MS_DSP_PROF_LOCAL type BAL_S_PROF .
  data MV_CALLSTACK_LVL type I .

  class-methods CLEAR_INSTANCE
    importing
      !IV_OBJECT type BALOBJ_D optional
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_EXTNUM type BALHDR-EXTNUMBER optional
    changing
      !CO_LOG_REF type ref to ZCL_BA_LOGGER optional .
  class-methods DB_SAVE_ALL
    importing
      !IV_IN_UPDATE_TASK type ABAP_BOOL default ABAP_FALSE .
  class-methods GET_INSTANCE
    importing
      !IV_OBJECT type BALOBJ_D optional
      !IV_SUBOBJECT type BALSUBOBJ optional
      !IV_EXTNUM type BALHDR-EXTNUMBER
      !IV_DB_SAVE type ABAP_BOOL default ABAP_TRUE
    returning
      value(RO_OBJ) type ref to ZCL_BA_LOGGER .
  methods ADD_BAL_MSG
    importing
      !IS_BAL_MSG type BAL_S_MSG .
  methods ADD_BAPIRET2
    importing
      !IS_BAPIRET2 type BAPIRET2
      !IS_CONTEXT type BAL_S_CONT optional .
  methods ADD_BAPIRET2_T
    importing
      !IT_BAPIRET2_T type BAPIRET2_T .
  methods ADD_BAPIRETURN
    importing
      !IS_BAPIRETURN type BAPIRETURN
      !IS_CONTEXT type BAL_S_CONT optional .
  methods ADD_BAPIRETURN1
    importing
      !IS_BAPIRETURN1 type BAPIRETURN1
      !IS_CONTEXT type BAL_S_CONT optional .
  methods ADD_BAPIRETURN1_TAB
    importing
      !IT_BAPIRETURN1_TAB type BAPIRETURN1_TABTYPE .
  methods ADD_BAPIRETURN_T
    importing
      !IT_BAPIRETURN type ICL_T_BAPIRETURN .
  methods ADD_FREE_TEXT
    importing
      !IV_MSGTY type SYMSGTY default 'E'
      !IV_PROBCLASS type BALPROBCL default '4'
      !IV_TEXT type CLIKE
      !IS_CONTEXT type BAL_S_CONT optional
    preferred parameter IV_MSGTY .
  methods ADD_SYMSG
    importing
      !IV_MSGID type SY-MSGID
      !IV_MSGTY type SY-MSGTY default 'E'
      !IV_MSGNO type SY-MSGNO
      !IV_MSGV1 type SYMSGV optional
      !IV_MSGV2 type SYMSGV optional
      !IV_MSGV3 type SYMSGV optional
      !IV_MSGV4 type SYMSGV optional
      !IS_CONTEXT type BAL_S_CONT optional .
  methods DB_SAVE
    importing
      !IV_IN_UPDATE_TASK type ABAP_BOOL default ABAP_FALSE .
  methods SET_CONTEXT_FIELDS
    importing
      !IT_CONTEXT type BAL_T_FCAT .
  methods SHOW_GLOBAL
    importing
      !IV_DATE_FROM type BALHDR-ALDATE optional
      !IV_TIME_FROM type BALHDR-ALTIME optional
      !IV_USE_INTERNAL_DATE type ABAP_BOOL optional
      !IV_MULTIPLE_EXTNUM type ABAP_BOOL optional
      !IV_WITHOUT_DATES type ABAP_BOOL default ABAP_TRUE
      !IV_ALUSER type BALHDR-ALUSER optional .
  methods SHOW_LOCAL
    importing
      !IV_TITLE type BALTITLE optional
      !IV_POPUP type ABAP_BOOL default ABAP_TRUE
    preferred parameter IV_TITLE .
protected section.
private section.

  types:
    BEGIN OF mty_s_instances,
        OBJECT TYPE BALOBJ_D,
        SUBOBJECT type BALSUBOBJ,
        exnum   TYPE balhdr-extnumber,
        obj_ref TYPE REF TO ZCL_BA_LOGGER,
    END OF mty_s_instances .
  types:
    mty_t_instances TYPE HASHED TABLE OF mty_s_instances WITH UNIQUE KEY OBJECT subobject exnum .

  class-data MT_INSTANCES type MTY_T_INSTANCES .
  class-data MV_DATE_FROM type BALHDR-ALDATE .
  class-data MV_TIME_FROM type BALHDR-ALTIME .
  data MS_LOG_HEADER type BAL_S_LOG .
  data MV_DB_SAVE type ABAP_BOOL .
  data MV_EXTNUM type BALHDR-EXTNUMBER .
  data MV_LOG_HANDLE type BALLOGHNDL .

  methods CONSTRUCTOR
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
      !IV_EXTNUM type BALHDR-EXTNUMBER
      !IV_DB_SAVE type ABAP_BOOL .
  methods LOG_CREATE
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
    returning
      value(RV_HANDLE) type BALLOGHNDL .
  methods LOG_GET_DSP_PROFILE
    importing
      !IV_POPUP type ABAP_BOOL default ABAP_FALSE
    returning
      value(RS_DISP_PROF) type BAL_S_PROF .
ENDCLASS.



CLASS ZCL_BA_LOGGER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAL_MSG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BAL_MSG                     TYPE        BAL_S_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bal_msg.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = mv_log_handle
      i_s_msg          = is_bal_msg
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BAPIRET2                    TYPE        BAPIRET2
* | [--->] IS_CONTEXT                     TYPE        BAL_S_CONT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapiret2.

  DATA: ls_bal_msg TYPE bal_s_msg.
  cl_ops_ehp_mapping_helper=>map_bapiret2_to_bal_s_msg( EXPORTING is_messages  = is_bapiret2
                                                        IMPORTING es_bal_s_msg = ls_bal_msg ).
  ls_bal_msg-context = is_context.
  add_bal_msg( ls_bal_msg ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRET2_T
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_BAPIRET2_T                  TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapiret2_t.

  FIELD-SYMBOLS: <ls_bapiret2> TYPE bapiret2.
  LOOP AT it_bapiret2_t ASSIGNING <ls_bapiret2>.
    add_bapiret2( is_bapiret2 = <ls_bapiret2> ).
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRETURN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BAPIRETURN                  TYPE        BAPIRETURN
* | [--->] IS_CONTEXT                     TYPE        BAL_S_CONT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapireturn.

  DATA: ls_bapiret2 TYPE bapiret2.
  CALL FUNCTION 'BALW_RETURN_TO_RET2'
    EXPORTING
      return_in = is_bapireturn
    IMPORTING
      return_ou = ls_bapiret2.
  add_bapiret2( is_bapiret2 = ls_bapiret2
                is_context  = is_context ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRETURN1
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BAPIRETURN1                 TYPE        BAPIRETURN1
* | [--->] IS_CONTEXT                     TYPE        BAL_S_CONT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapireturn1.
  DATA: ls_bapiret2 TYPE bapiret2.

  CALL FUNCTION 'BALW_RET1_TO_RET2'
    EXPORTING
      return_in = is_bapireturn1
    IMPORTING
      return_ou = ls_bapiret2.

  add_bapiret2( is_bapiret2 = ls_bapiret2
                is_context  = is_context ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRETURN1_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_BAPIRETURN1_TAB             TYPE        BAPIRETURN1_TABTYPE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapireturn1_tab.
  FIELD-SYMBOLS: <ls_bapireturn1> TYPE bapireturn1.

  LOOP AT it_bapireturn1_tab ASSIGNING <ls_bapireturn1>.
    add_bapireturn1( <ls_bapireturn1> ).
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_BAPIRETURN_T
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_BAPIRETURN                  TYPE        ICL_T_BAPIRETURN
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_bapireturn_t.

  FIELD-SYMBOLS: <ls_bapireturn> TYPE bapireturn.
  LOOP AT it_bapireturn ASSIGNING <ls_bapireturn>.
    add_bapireturn( is_bapireturn = <ls_bapireturn> ).
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_FREE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSGTY                       TYPE        SYMSGTY (default ='E')
* | [--->] IV_PROBCLASS                   TYPE        BALPROBCL (default ='4')
* | [--->] IV_TEXT                        TYPE        CLIKE
* | [--->] IS_CONTEXT                     TYPE        BAL_S_CONT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_free_text.

  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle     = mv_log_handle
      i_msgty          = iv_msgty
      i_probclass      = iv_probclass
      i_text           = iv_text
      i_s_context      = is_context
*     I_S_PARAMS       =
*     I_DETLEVEL       = '1'
*   IMPORTING
*     E_S_MSG_HANDLE   =
*     E_MSG_WAS_LOGGED =
*     E_MSG_WAS_DISPLAYED       =
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->ADD_SYMSG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MSGID                       TYPE        SY-MSGID
* | [--->] IV_MSGTY                       TYPE        SY-MSGTY (default ='E')
* | [--->] IV_MSGNO                       TYPE        SY-MSGNO
* | [--->] IV_MSGV1                       TYPE        SYMSGV(optional)
* | [--->] IV_MSGV2                       TYPE        SYMSGV(optional)
* | [--->] IV_MSGV3                       TYPE        SYMSGV(optional)
* | [--->] IV_MSGV4                       TYPE        SYMSGV(optional)
* | [--->] IS_CONTEXT                     TYPE        BAL_S_CONT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_symsg.

  DATA: ls_bapiret2 TYPE bapiret2,
        lv_msgv1    TYPE sy-msgv1,
        lv_msgv2    TYPE sy-msgv2,
        lv_msgv3    TYPE sy-msgv3,
        lv_msgv4    TYPE sy-msgv4.

  lv_msgv1 = iv_msgv1.
  lv_msgv2 = iv_msgv2.
  lv_msgv3 = iv_msgv3.
  lv_msgv4 = iv_msgv4.
  CONDENSE: lv_msgv1,
            lv_msgv2,
            lv_msgv3,
            lv_msgv4.
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = iv_msgty
      cl     = iv_msgid
      number = iv_msgno
      par1   = lv_msgv1
      par2   = lv_msgv2
      par3   = lv_msgv3
      par4   = lv_msgv4
*     LOG_NO = ' '
*     LOG_MSG_NO       = ' '
*     PARAMETER        = ' '
*     ROW    = 0
*     FIELD  = ' '
    IMPORTING
      return = ls_bapiret2.
  add_bapiret2( is_bapiret2 = ls_bapiret2
                is_context  = is_context ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BA_LOGGER=>CLEAR_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D(optional)
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ(optional)
* | [--->] IV_EXTNUM                      TYPE        BALHDR-EXTNUMBER(optional)
* | [<-->] CO_LOG_REF                     TYPE REF TO ZCL_BA_LOGGER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD clear_instance.
  FIELD-SYMBOLS: <ls_instance> TYPE mty_s_instances.
  IF iv_extnum IS SUPPLIED OR iv_object IS SUPPLIED OR iv_subobject IS SUPPLIED.
    READ TABLE mt_instances WITH TABLE KEY object = iv_object
                                           subobject = iv_subobject
                                           exnum = iv_extnum
                                 ASSIGNING <ls_instance>.
  ELSEIF co_log_ref IS SUPPLIED.
    READ TABLE mt_instances WITH TABLE KEY object = co_log_ref->ms_log_header-object
                                           subobject = co_log_ref->ms_log_header-subobject
                                           exnum = co_log_ref->ms_log_header-extnumber
                                 ASSIGNING <ls_instance>.
  ENDIF.
  IF sy-subrc = 0.
    FREE: <ls_instance>-obj_ref,
          co_log_ref.
    DELETE TABLE mt_instances FROM <ls_instance>.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BA_LOGGER->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ
* | [--->] IV_EXTNUM                      TYPE        BALHDR-EXTNUMBER
* | [--->] IV_DB_SAVE                     TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD constructor.

  mv_callstack_lvl = 4.
  mv_extnum = iv_extnum.
  IF iv_object IS NOT INITIAL.
    mv_db_save = iv_db_save.
  ENDIF.
  mv_log_handle = log_create( iv_object = iv_object
                              iv_subobject = iv_subobject ).
  ms_dsp_prof_local = log_get_dsp_profile( iv_popup = abap_true ).
  ms_dsp_prof_global = log_get_dsp_profile( iv_popup = abap_false ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->DB_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IN_UPDATE_TASK              TYPE        ABAP_BOOL (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD db_save.

  DATA: lt_log_handle TYPE bal_t_logh.
  CHECK mv_db_save = abap_true.

  APPEND mv_log_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_log_handle
      i_in_update_task = iv_in_update_task
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BA_LOGGER=>DB_SAVE_ALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IN_UPDATE_TASK              TYPE        ABAP_BOOL (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD db_save_all.
  FIELD-SYMBOLS: <ls_instance> TYPE mty_s_instances.
  LOOP AT mt_instances ASSIGNING <ls_instance>.
    <ls_instance>-obj_ref->db_save( iv_in_update_task ).
  ENDLOOP.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BA_LOGGER=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D(optional)
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ(optional)
* | [--->] IV_EXTNUM                      TYPE        BALHDR-EXTNUMBER
* | [--->] IV_DB_SAVE                     TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] RO_OBJ                         TYPE REF TO ZCL_BA_LOGGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD get_instance.

  DATA: ls_instance TYPE mty_s_instances.
  READ TABLE mt_instances WITH TABLE KEY object = iv_object
                                         subobject = iv_subobject
                                         exnum = iv_extnum
                          INTO ls_instance.
  IF ls_instance-obj_ref IS NOT BOUND.
    " Need to set time filters ones, to show log from earliest
    IF mv_date_from IS INITIAL AND mv_time_from IS INITIAL.
      mv_date_from = sy-datum. " For correct finding of logs when showing
      mv_time_from = sy-uzeit. " For correct finding of logs when showing
    ENDIF.
    CREATE OBJECT ro_obj
      EXPORTING
        iv_object    = iv_object
        iv_subobject = iv_subobject
        iv_extnum    = iv_extnum
        iv_db_save   = iv_db_save.
    ls_instance-object = iv_object.
    ls_instance-subobject = iv_subobject.
    ls_instance-exnum = iv_extnum.
    ls_instance-obj_ref = ro_obj.
    INSERT ls_instance INTO TABLE mt_instances.
  ELSE.
    ls_instance-obj_ref->mv_db_save = iv_db_save.
    ro_obj = ls_instance-obj_ref.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BA_LOGGER->LOG_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        BALOBJ_D
* | [--->] IV_SUBOBJECT                   TYPE        BALSUBOBJ
* | [<-()] RV_HANDLE                      TYPE        BALLOGHNDL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD log_create.

  ms_log_header-object = iv_object.
  ms_log_header-subobject = iv_subobject.
  ms_log_header-extnumber = mv_extnum.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ms_log_header
    IMPORTING
      e_log_handle = rv_handle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_BA_LOGGER->LOG_GET_DSP_PROFILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_POPUP                       TYPE        ABAP_BOOL (default =ABAP_FALSE)
* | [<-()] RS_DISP_PROF                   TYPE        BAL_S_PROF
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD log_get_dsp_profile.
  IF iv_popup = abap_true.
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = rs_disp_prof
      EXCEPTIONS
        OTHERS              = 0.
    rs_disp_prof-use_grid = abap_true.
*    rs_disp_prof-no_toolbar = abap_true.
*    rs_disp_prof-title = mc_slg_title.
  ELSE.
    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = rs_disp_prof.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->SET_CONTEXT_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CONTEXT                     TYPE        BAL_T_FCAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD set_context_fields.
  APPEND LINES OF: it_context TO ms_dsp_prof_global-mess_fcat,
                   it_context TO ms_dsp_prof_local-mess_fcat.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->SHOW_GLOBAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_FROM                   TYPE        BALHDR-ALDATE(optional)
* | [--->] IV_TIME_FROM                   TYPE        BALHDR-ALTIME(optional)
* | [--->] IV_USE_INTERNAL_DATE           TYPE        ABAP_BOOL(optional)
* | [--->] IV_MULTIPLE_EXTNUM             TYPE        ABAP_BOOL(optional)
* | [--->] IV_WITHOUT_DATES               TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] IV_ALUSER                      TYPE        BALHDR-ALUSER(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD show_global.

  DATA: ls_log_filter  TYPE bal_s_lfil,
        lt_log_header  TYPE balhdr_t,
        lt_log_handle  TYPE bal_t_logh,
        ls_disp_global TYPE bal_s_prof.
  DATA: lv_date_from TYPE balhdr-aldate,
        lv_time_from TYPE balhdr-altime,
        lv_extnum    TYPE balhdr-extnumber.
  IF iv_multiple_extnum IS INITIAL.
    lv_extnum = mv_extnum.
  ENDIF.
  IF iv_without_dates = abap_false.
    IF iv_date_from IS NOT INITIAL.
      lv_date_from = iv_date_from.
      lv_time_from = iv_time_from.
    ELSE.
      lv_date_from = mv_date_from.
      lv_time_from = mv_time_from.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'BAL_FILTER_CREATE'
    EXPORTING
      i_object       = ms_log_header-object
      i_subobject    = ms_log_header-subobject
      i_extnumber    = lv_extnum
      i_aldate_from  = lv_date_from
      i_aldate_to    = sy-datum
      i_altime_from  = lv_time_from
      i_altime_to    = sy-uzeit
      i_aluser       = iv_aluser
    IMPORTING
      e_s_log_filter = ls_log_filter.
  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter     = ls_log_filter
    IMPORTING
      e_t_log_header     = lt_log_header
    EXCEPTIONS
      log_not_found      = 1
      no_filter_criteria = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ELSE.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = lt_log_header
      IMPORTING
        e_t_log_handle     = lt_log_handle
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = ms_dsp_prof_global
      i_t_log_handle       = lt_log_handle
      i_s_log_filter       = ls_log_filter
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BA_LOGGER->SHOW_LOCAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TITLE                       TYPE        BALTITLE(optional)
* | [--->] IV_POPUP                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD show_local.
  DATA: lt_log_handle TYPE bal_t_logh,
        ls_dsp_prof   TYPE bal_s_prof.

  ms_dsp_prof_local-title = iv_title.
  ms_dsp_prof_global-title = iv_title.
  IF iv_popup = abap_false.
    ls_dsp_prof = ms_dsp_prof_global.
  ELSE.
    ls_dsp_prof = ms_dsp_prof_local.
  ENDIF.
  APPEND mv_log_handle TO lt_log_handle.
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = ls_dsp_prof
      i_t_log_handle       = lt_log_handle
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.
ENDCLASS.