*&---------------------------------------------------------------------*
*& Report ZTCODEREM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTCODE_REM_PROG_1.


DATA: gt_bdcdata TYPE STANDARD TABLE OF bdcdata.
DATA: gt_messtab TYPE STANDARD TABLE OF bdcmsgcoll.
DATA: ls_messtab TYPE bdcmsgcoll.
DATA: ls_ctu_params TYPE ctu_params.
DATA: lv_message TYPE string.

PARAMETERS : p_bu    TYPE bu_partner,
             p_name1 TYPE char50,
             p_name2 TYPE char50,
             p_name3 TYPE char50,
             p_name4 TYPE char50,
             p_sort  TYPE char50.


START-OF-SELECTION.

  ls_ctu_params-dismode = 'A'.
  ls_ctu_params-nobinpt = 'X'.
  ls_ctu_params-updmode = 'A'.

 PERFORM bdc_dynpro      USING 'SAPMF02D'    '7101'.
  PERFORM bdc_field       USING 'BDC_OKCODE'  '=ENTR'.
  PERFORM bdc_field       USING 'RF02D-KUNNR' p_bu.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=TAB01'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=$2OC'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=$3OC'.

  PERFORM bdc_dynpro      USING 'SAPMF02D'   '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=TAB02'.

** Personal Data
  PERFORM bdc_field       USING 'SZA1_D0100-TITLE_MEDI'  '0003'.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'       p_name1.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME2'       p_name2.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME3'       p_name3.
  PERFORM bdc_field       USING 'ADDR1_DATA-NAME4'       p_name4.
  PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'       p_sort.


** Address Data
  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'     'aA'.
  PERFORM bdc_field       USING 'ADDR1_DATA-HOUSE_NUM1' '1'.
  PERFORM bdc_field       USING 'ADDR1_DATA-HOUSE_NUM2' '23'.
  PERFORM bdc_field       USING 'ADDR1_DATA-CITY2'      'A'.
  PERFORM bdc_field       USING 'ADDR1_DATA-HOME_CITY'  '23'.
  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1' '500053'.
  PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'      'hyderabad'.
  PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'    'IN'.
  PERFORM bdc_field       USING 'ADDR1_DATA-REGION'     '01'.
  PERFORM bdc_field       USING 'ADDR1_DATA-TRANSPZONE' 'Z0000001'.
  PERFORM bdc_field       USING 'ADDR1_DATA-DONT_USE_S' '0009'.
  PERFORM bdc_field       USING 'ADDR1_DATA-PO_BOX'     '500053'.
  PERFORM bdc_field       USING 'ADDR1_DATA-PO_BOX_LOBBY' 'wew'.
  PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE2'  '50005'.
  PERFORM bdc_field       USING 'ADDR1_DATA-PO_BOX_CTY'  'US'.
  PERFORM bdc_field       USING 'ADDR1_DATA-PO_BOX_REG'  'AK'.
  PERFORM bdc_field       USING 'ADDR1_DATA-DONT_USE_P'  '0009'.
  PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'       'EN'.
  PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'  '12'.
  PERFORM bdc_field       USING 'SZA1_D0100-TEL_EXTENS'  '11'.
  PERFORM bdc_field       USING 'SZA1_D0100-MOB_NUMBER'  '12'.
  PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'  '232'.
  PERFORM bdc_field       USING 'SZA1_D0100-FAX_EXTENS'  '11'.
  PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'    '12@gmail.com'.


  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=TAB03'.

  PERFORM bdc_field       USING 'KNA1-LIFNR' '3100000003'.
  PERFORM bdc_field       USING 'KNA1-BEGRU' '001'.
  PERFORM bdc_field       USING 'KNA1-VBUND' '1'.
  PERFORM bdc_field       USING 'KNA1-KONZS' '111'.
  PERFORM bdc_field       USING 'KNA1-BBBNR' '0000012'.
  PERFORM bdc_field       USING 'KNA1-BBSNR' '00223'.
  PERFORM bdc_field       USING 'KNA1-BUBKZ' '2'.
  PERFORM bdc_field       USING 'KNA1-BRSCH' '0001'.
  PERFORM bdc_field       USING 'KNA1-BAHNS' 'ssd'.
  PERFORM bdc_field       USING 'KNA1-BAHNE' 'sds'.
  PERFORM bdc_field       USING 'KNA1-LOCCO' '33'.
  PERFORM bdc_field       USING 'KNA1-STCD1' '12'.
  PERFORM bdc_field       USING 'KNA1-STCD2' '12'.
  PERFORM bdc_field       USING 'KNA1-FISKN' '178'.
  PERFORM bdc_field       USING 'KNA1-COUNC' 'IN'.
  PERFORM bdc_field       USING 'KNA1-STCEG' '11'.
  PERFORM bdc_field       USING 'KNA1-STCD5' '121'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=IBAN'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'IBAN(01)'.
  PERFORM bdc_field       USING 'KNBK-BANKS(01)' 'IN'.
  PERFORM bdc_field       USING 'KNBK-BANKL(01)' '1110'.
  PERFORM bdc_field       USING 'KNBK-BANKN(01)' '11111'.
  PERFORM bdc_field       USING 'KNBK-BKONT(01)' 'AA'.
  PERFORM bdc_field       USING 'KNBK-KOINH(01)' 'AAests'.

  PERFORM bdc_dynpro      USING 'SAPLIBMA' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/E'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=IBAN'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'IBAN(02)'.
  PERFORM bdc_field       USING 'KNBK-BANKS(02)' 'IN'.
  PERFORM bdc_field       USING 'KNBK-BANKL(02)' '1120'.
  PERFORM bdc_field       USING 'KNBK-BANKN(02)' '11111'.
  PERFORM bdc_field       USING 'KNBK-BKONT(02)' 'BB'.
  PERFORM bdc_field       USING 'KNBK-KOINH(02)' 'AAests11'.

*GT_BUT0CC-CCNUM_MASK

  PERFORM bdc_dynpro      USING 'SAPLIBMA' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/E'.

  PERFORM bdc_dynpro      USING 'SAPMF02D' '7000'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=UPDA'.


  CALL TRANSACTION 'XD02' USING gt_bdcdata OPTIONS FROM ls_ctu_params MESSAGES INTO gt_messtab.

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
