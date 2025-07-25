*&---------------------------------------------------------------------*
*& Report ZAAVH_TRAN_MIG_S4_A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAAVH_TRAN_MIG_S4_C_1.


DATA: gt_bdcdata TYPE STANDARD TABLE OF bdcdata.
DATA: gt_messtab TYPE STANDARD TABLE OF bdcmsgcoll.
DATA: ls_messtab TYPE bdcmsgcoll.
DATA: lv_message TYPE string.

PARAMETERS : p_bu    TYPE bu_partner,
             p_name1 TYPE char50,
             p_name2 TYPE char50,
             p_name3 TYPE char50,
             p_name4 TYPE char50,
             p_sort  TYPE char50.


START-OF-SELECTION.

  PERFORM bdc_dynpro      USING 'SAPMF02D'    '0101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '/00'.
  PERFORM bdc_field       USING 'RF02D-KUNNR' p_bu.
  PERFORM bdc_field       USING 'RF02D-D0110' 'X'.

  PERFORM bdc_dynpro      USING 'SAPMF02D'   '0110'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=UPDA'.
  PERFORM bdc_field       USING 'KNA1-NAME1' p_name1.
  PERFORM bdc_field       USING 'KNA1-NAME2' p_name2.
  PERFORM bdc_field       USING 'KNA1-NAME3' p_name3.
  PERFORM bdc_field       USING 'KNA1-NAME4' p_name4.
  PERFORM bdc_field       USING 'KNA1-SORTL' p_sort.

  CALL TRANSACTION 'XD02' USING gt_bdcdata UPDATE 'A' MESSAGES INTO gt_messtab.

END-OF-SELECTION.

  LOOP AT gt_messtab INTO ls_messtab.

    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = ls_messtab-msgid
        lang      = sy-langu
        no        = ls_messtab-msgnr
        v1        = ls_messtab-msgv1
        v2        = ls_messtab-msgv2
        v3        = ls_messtab-msgv3
        v4        = ls_messtab-msgv4
      IMPORTING
        msg       = lv_message
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    WRITE: lv_message.
    CLEAR lv_message.
  ENDLOOP.


*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*      -->P_0028   text
*----------------------------------------------------------------------*
FORM bdc_dynpro  USING p_program
                       p_dynpro.
  DATA: ls_bdcdata TYPE bdcdata.
  ls_bdcdata-program  = p_program.
  ls_bdcdata-dynpro   = p_dynpro.
  ls_bdcdata-dynbegin = abap_true.
  APPEND ls_bdcdata TO gt_bdcdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0077   text
*      -->P_0078   text
*----------------------------------------------------------------------*
FORM bdc_field  USING p_fnam
                      p_fval.
  DATA: ls_bdcdata TYPE bdcdata.
  IF p_fval IS NOT INITIAL.
    ls_bdcdata-fnam = p_fnam.
    ls_bdcdata-fval = p_fval.
    APPEND ls_bdcdata TO gt_bdcdata.
  ENDIF.
ENDFORM.
