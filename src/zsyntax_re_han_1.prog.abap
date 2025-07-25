*&---------------------------------------------------------------------*
*& Report ZSYNTAX_RE_HAN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSYNTAX_RE_HAN_1.


TABLES: taplp , tadir .

TYPES: BEGIN OF ty_dip,
         count_no TYPE /vshaneya/zprg-count_no, " Natural Number
         obsolete TYPE /vshaneya/zprg-obsolete, " Obsolete statements/function modules
         zreplace TYPE /vshaneya/zprg-zreplace, " replace statements/function modules
         stfm     TYPE /vshaneya/zprg-stfm,     " statements/function module
         zprogram TYPE /vshaneya/zprg-zprogram, " Interface Name
         line     TYPE /vshaneya/zprg-line,
       END OF ty_dip.

TYPES: BEGIN OF ty_tadir,
         obj_name TYPE tadir-obj_name,                " Object Name in Object Directory
       END OF ty_tadir.
TYPES: BEGIN OF ty_old_par,
         par TYPE string,
         val TYPE string,
       END OF ty_old_par,
       ty_t_old_par TYPE STANDARD TABLE OF ty_old_par.
*----------------------------------------------------------------------*
*                    CONSTANTS
*----------------------------------------------------------------------*
DATA : l_exit TYPE i.
DATA: it_param          TYPE abap_parmdescr_tab,
      it_exce           TYPE abap_excpdescr_tab,
      it_seosubcodf     TYPE /vshaneya/tt_seosubcodf,
      l_i_obj_class_att TYPE REF TO /vshaneya/zclass_attr.
DATA : l_code1 TYPE char30,
       lv_int  TYPE char3,
       lv_num  TYPE char3,
       l_code2 TYPE char15.
DATA: l_tabix TYPE i.
DATA: lv_index TYPE char1.
DATA : l_upload        TYPE char1,
       l_popup_message TYPE char1,
       l_filename      TYPE char1,
       l_namestab      TYPE char1,
       l_download      TYPE char1.
DATA c_x TYPE c VALUE 'X'.

*----------------------------------------------------------------------*
*             VARIABLES
*----------------------------------------------------------------------*
DATA : l_var1 TYPE string,
       l_var2 TYPE string.
*DATA: lv_count TYPE int4 VALUE 1.
DATA lv_ps1 TYPE string.
DATA lv_ps2 TYPE string.
DATA :gv_f1 TYPE string,
      gv_f2 TYPE string,
      gv_f3 TYPE string,
      gv_f4 TYPE string.
DATA gv_move1 TYPE string .
DATA gv_move2 TYPE string.
DATA gv_pack1 TYPE string .
DATA gv_pack2 TYPE string.
DATA gv_ptabix TYPE sy-tabix.
DATA gv_tabix  TYPE sy-tabix.
DATA param  TYPE string.
DATA param2 TYPE string.
DATA param3 TYPE string.
DATA param4 TYPE string.
DATA param5 TYPE string.
DATA param6 TYPE string.
DATA lv_tab1 TYPE sy-tabix.
DATA gt_code TYPE TABLE OF string.
DATA gs_code TYPE string.
DATA gv_c TYPE c.
DATA gv_acct TYPE c.
DATA gv_act_det TYPE c.
DATA: gv_imp      TYPE c,
      gv_chg      TYPE c,
      gv_tab      TYPE c,
      gv_exp      TYPE c,
      gv_objpart  TYPE c,
      gv_objpartx TYPE c.

DATA gv_objc TYPE c.
DATA gv_ins_gt TYPE c.
DATA gv_accit TYPE c.
DATA gv_amco TYPE c.
*--------------------------------------------------------------------*
*             INTERNAL TABLES & WORK AREA
*--------------------------------------------------------------------*
DATA: it_dip   TYPE TABLE OF ty_dip,
      it_tadir TYPE TABLE OF ty_tadir,
      wa_dip   TYPE ty_dip,
      wa_tadir TYPE ty_tadir.
*changes for dashboard done by nikhila
DATA: lv_count  TYPE int4 VALUE 1,
      wa_table1 TYPE /vshaneya/table1,
      wa_table2 TYPE /vshaneya/table2,
      wa_table  TYPE /vshaneya/stable,
      wa_stable TYPE /vshaneya/stable.
*end of changes

DATA : it_code  TYPE TABLE OF string,
       it_code1 TYPE TABLE OF string,
       wa_code  TYPE string,
       wa_code1 TYPE string.

TYPES : BEGIN OF ty_codet,
          code TYPE string,
          line TYPE sy-tabix,
        END OF ty_codet.
DATA it_codet TYPE TABLE OF ty_codet.
DATA: wa_codet   TYPE ty_codet,
      wa_old_par TYPE ty_old_par,
      it_old_par TYPE ty_t_old_par.

DATA lt_report TYPE STANDARD TABLE OF trdir.
DATA ls_report TYPE trdir.

DATA lv_program TYPE rs38m-programm.
DATA grs_sdesc TYPE REF TO cl_abap_structdescr.
DATA grd_sdesc TYPE REF TO cl_abap_structdescr.
DATA gss_comp TYPE abap_compdescr.
DATA gsd_comp TYPE abap_compdescr.

TYPES: BEGIN OF ty_reposrc,
         progname TYPE progname,
       END OF ty_reposrc,
       ty_t_reposrc TYPE SORTED TABLE OF ty_reposrc WITH NON-UNIQUE KEY progname.
DATA: l_is_reposrc TYPE ty_t_reposrc.

TYPES: BEGIN OF t_itab,
         code_word(80) TYPE c,
       END OF t_itab.
DATA: itab TYPE TABLE OF t_itab,
      wa   TYPE t_itab.

DATA: lv_code1 TYPE string,
      lv_code2 TYPE string.
*--------------------------------------------------------------------*
*                SELECTION SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-163.
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 2(20) TEXT-162.
SELECT-OPTIONS : s_appl FOR taplp-appl,
s_prog FOR lv_program  OBLIGATORY.
SELECT-OPTIONS : s_pack FOR tadir-devclass.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS:p_report TYPE xflag RADIOBUTTON GROUP rb1 DEFAULT 'X',
           p_rem    TYPE xflag RADIOBUTTON GROUP rb1,
           p_rev    TYPE xflag RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
*             START OF SELECTION
*--------------------------------------------------------------------*
START-OF-SELECTION.
  REFRESH it_code.
  IF s_prog[] IS INITIAL.
    MESSAGE TEXT-160 TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  DELETE FROM /vshaneya/zprg.
  CLEAR: l_upload,
         l_popup_message,
         l_filename,
         l_namestab,
         l_download.
  IF s_appl[] IS NOT INITIAL.
    s_appl-low = '*'.
    APPEND s_appl TO s_appl[].

    SELECT progname
      FROM reposrc AS r
      INNER JOIN tadir AS t
      ON r~progname = t~obj_name
      INTO TABLE l_is_reposrc
      WHERE r~progname IN s_prog
      AND r~appl IN s_appl
      AND t~devclass IN s_pack.
  ENDIF.
  IF l_is_reposrc IS NOT INITIAL.
    SELECT * FROM trdir
      INTO TABLE lt_report
      FOR ALL ENTRIES IN l_is_reposrc
      WHERE name = l_is_reposrc-progname.
  ELSE.
    SELECT * FROM trdir INTO TABLE lt_report WHERE name IN s_prog.
  ENDIF.

  LOOP AT lt_report INTO ls_report.

    lv_count = lv_count + 1.
    READ REPORT ls_report-name INTO it_code.

    IF NOT it_code[] IS INITIAL.
      CLEAR : l_tabix, l_upload.

      gt_code[] = it_code[].

      DATA lv_code LIKE wa_code.

      LOOP AT it_code INTO wa_code.
        CLEAR lv_code .
        lv_code = wa_code.
**************************START OF REPLACE OBSOLETE STATEMENTS***************************************************
        TRANSLATE wa_code TO UPPER CASE.
        IF wa_code CS 'LIKE' AND wa_code NS 'LIKE LINE OF' OR wa_code CS 'STOP' OR wa_code CS '"#EC NEEDED'
          OR wa_code CS 'LOOP AT SCREEN '       OR wa_code CS 'MODIFY SCREEN ' OR wa_code CS 'MOVE'
          OR wa_code CS 'UNICODE ENABLING' OR wa_code CS 'SET EXTENDED CHECK OFF' OR wa_code CS 'SET EXTENDED CHECK ON'
          OR wa_code CS 'TABLES'        OR wa_code CS 'SET LOCALE' OR wa_code  CS 'GET LOCALE' OR wa_code CS 'CALL METHOD'
          OR wa_code CS 'PACK'          OR wa_code CS 'TYPE-POOLS' OR wa_code CS 'LOAD'    OR wa_code CS 'FIELDS' OR wa_code CS 'LOOP AT'
          OR wa_code CS 'REFRESH'       OR wa_code CS 'PARAMETER'  OR wa_code CS 'PERFORM' OR wa_code CS 'CALL FUNCTION'
          OR wa_code CS 'CORRESPONDING' OR wa_code CS 'DETAIL'     OR wa_code CS 'SUMMARY' OR wa_code CS 'INPUT'
          OR wa_code CS 'REPLACE'       OR wa_code CS 'SEARCH'     OR wa_code CS 'ASSIGN' OR wa_code CS 'SORT'
          OR wa_code CS 'AT NEW'   OR wa_code CS 'WRITE' OR wa_code CS '><' OR wa_code CS '=<' OR wa_code CS 'ADD'
          OR wa_code CS 'SUBTRACT' OR wa_code CS 'MULTIPLY' OR wa_code CS 'DIVIDE' OR wa_code CS 'IS REQUESTED'
          OR wa_code CS 'ASSIGN LOCAL COPY OF' OR wa_code CS 'CALL TRANSACTION' OR wa_code CS 'SELECT SINGLE' OR
          wa_code CS 'INSERT' OR wa_code CS 'UPDATE' OR wa_code CS 'DELETE' OR wa_code CS 'SELECT'
          OR wa_code CS 'SELECT-OPTIONS' OR wa_code CS 'WAIT UNTIL' OR wa_code CS 'OCCURS' .
          CONDENSE wa_code.
          lv_count = wa_dip-count_no.
          IF wa_code CS 'LIKE' .
            REPLACE ALL OCCURRENCES OF 'LIKE' IN wa_code WITH 'TYPE'.
            MODIFY it_code FROM wa_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'LIKE'.
              wa_dip-zreplace =  'TYPE'.
              wa_dip-stfm = 'STATEMENT REPLACED'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
          ELSEIF  wa_code+0(4) CS 'STOP' AND wa_code+0(1) NE '*'..
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'STOP' IN wa_code WITH 'LEAVE TO SCREEN SY-DYNNR. "Added by Haneya Tool'(166).
            INSERT wa_code INTO it_code..

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'STOP'.
              wa_dip-zreplace =  'LEAVE  TO SCREEN SY-DYNNR'.
              wa_dip-stfm = 'STATEMENT REPLACED'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.

              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.

          ELSEIF  wa_code CS 'WRITE' AND wa_code+0(1) NE '*' AND wa_code CS 'INDEX'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            DATA lv_w1 TYPE string.
            DATA lv_w2 TYPE string.
            DATA lv_w3 TYPE string.
            DATA lv_w4 TYPE string.
            DATA lv_w5 TYPE string.
            DATA lv_w6 TYPE string.

            DATA lv_w7 TYPE string.

            SPLIT wa_code  AT ' ' INTO lv_w1 lv_w2 lv_w3 lv_w4 lv_w5 lv_w6 lv_w7.
            CONCATENATE 'FIELD-SYMBOLS: <line> like line of ' lv_w4 '.' ' "Added by Haneya Tool' INTO wa_code SEPARATED BY space.

            INSERT wa_code INTO it_code.
            CONCATENATE 'assign' lv_w4 '[' lv_w6 ']' 'TO <LINE>.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.

            INSERT wa_code INTO it_code.
            CONCATENATE 'WRITE' lv_w2 'TO <LINE>' lv_w7 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.

            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'WRITE ... INDEX'.
              wa_dip-zreplace =  'WRITE dobj TO <line>[+off][(len)]'.
              wa_dip-stfm = 'WRITE is replaced with WRITE dobj TO <line>[+off][(len)]'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.

          ELSEIF wa_code CS '"#EC NEEDED'.
            CONCATENATE '*' wa_code INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"#EC NEEDED' IN wa_code WITH '##NEEDED. "Added by Haneya Tool'.
            MODIFY it_code FROM wa_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = '"#EC NEEDED'.
              wa_dip-zreplace =  '##NEEDED.'.
              wa_dip-stfm = 'Psedo Code Replaced'(164).
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'CALL TRANSACTION' AND wa_code NS 'WITH AUTHORITY-CHECK' AND  wa_code+0(1) NE '*'..
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            IF wa_code CS 'USING'.
              REPLACE ALL OCCURRENCES OF 'USING' IN wa_code WITH  'WITH AUTHORITY-CHECK USING '.
              CONCATENATE wa_code  '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            ELSE.
              CONCATENATE wa_code 'WITH AUTHORITY-CHECK'  '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            ENDIF.
            INSERT  wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'CALL TRANSACTION'.
              wa_dip-zreplace =  ' Addition WITH AUTHORITY-CHECK.'.
              wa_dip-stfm = 'Call.. Addition WITH AUTHORITY-CHECK'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes

            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'WITH HEADER LINE' AND  wa_code+0(1) NE '*' AND wa_code CS 'STANDARD TABLE OF'.

            REPLACE ALL OCCURRENCES OF '"ADDED BY HANEYA TOOL' IN wa_code WITH ''.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.


            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.

            REPLACE ALL OCCURRENCES OF 'WITH HEADER LINE' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            CONCATENATE wa_code '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT  wa_code INTO it_code.
            REPLACE ALL OCCURRENCES OF 'STANDARD TABLE OF' IN wa_code WITH ''.
            INSERT  wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'With Header line'.
              wa_dip-zreplace =  ' Removed with header line'.
              wa_dip-stfm = 'Created the external work area'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes

            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'OCCURS' AND   wa_code+0(1) NE '*'.
            DATA lv_c11 TYPE string.
            DATA lv_c12 TYPE string.
            DATA lv_c13 TYPE string.
            DATA lv_st TYPE c.
            DATA lv_wa TYPE string.
            REPLACE ALL OCCURRENCES OF '"ADDED BY HANEYA TOOL' IN wa_code WITH ''.
            IF wa_code CS '.' .
              IF wa_code CS 'WITH HEADER LINE'.
                lv_st = 'X'.
              ENDIF.
              CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
              MODIFY it_code FROM wa_code.
              REPLACE ALL OCCURRENCES OF 'OCCURS 0' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF 'WITH HEADER LINE' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF 'DATA ' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF ':' IN wa_code WITH ''.

              REPLACE ALL OCCURRENCES OF 'OCCURS 0' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.


              CONDENSE wa_code.
              SPLIT wa_code AT space INTO lv_c11 lv_c12 lv_c13.
              CLEAR wa_code.
              IF lv_st EQ 'X'.
                CONCATENATE 'wa_' lv_c11 INTO lv_wa.
                CONCATENATE wa_code 'DATA:' lv_wa  'TYPE '  lv_c13   '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
                INSERT  wa_code INTO it_code.
                CLEAR :lv_st,wa_code,lv_wa.

              ENDIF.
              CONCATENATE  'DATA:' lv_c11 'TYPE STANDARD TABLE OF'  lv_c13   '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
              INSERT  wa_code INTO it_code.
              IF sy-subrc = 0.
                wa_dip-count_no = lv_count + 1.
                wa_dip-obsolete = 'OCCURS'.
                wa_dip-zreplace =  'STANDARD TABLE OF..'.
                wa_dip-stfm = 'OCCURS IS REPLACED WITH STANDARD TABLE OF'.
                wa_dip-zprogram = ls_report-name.
                wa_dip-line = sy-tabix.
                APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                wa_stable-sno = wa_dip-count_no.
                wa_stable-objname = ls_report-name.
                wa_stable-processed = 'X'.
                INSERT /vshaneya/stable FROM wa_stable.
                CLEAR wa_stable.
*end of changes
              ENDIF.
              CLEAR wa_code.
            ELSEIF wa_code CS ',' AND wa_code CS 'DATA'.
              CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
              MODIFY it_code FROM wa_code.
              REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF ',' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF 'DATA ' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF ':' IN wa_code WITH ''.

              REPLACE ALL OCCURRENCES OF 'OCCURS 0' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.


              CONDENSE wa_code.
              SPLIT wa_code AT space INTO lv_c11 lv_c12 lv_c13.
              CLEAR wa_code.

              CONCATENATE wa_code 'DATA:' lv_c11 'TYPE STANDARD TABLE OF'  lv_c13   ',' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
              INSERT  wa_code INTO it_code.
              IF sy-subrc = 0.
                wa_dip-count_no = lv_count + 1.
                wa_dip-obsolete = 'OCCURS'.
                wa_dip-zreplace =  'STANDARD TABLE OF..'.
                wa_dip-stfm = 'OCCURS IS REPLACED WITH STANDARD TABLE OF'.
                wa_dip-zprogram = ls_report-name.
                wa_dip-line = sy-tabix.
                APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                wa_stable-sno = wa_dip-count_no.
                wa_stable-objname = ls_report-name.
                wa_stable-processed = 'X'.
                INSERT /vshaneya/stable FROM wa_stable.
                CLEAR wa_stable.
*end of changes
              ENDIF.
            ELSEIF wa_code CS ',' AND wa_code NS 'DATA'.

              CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
              MODIFY it_code FROM wa_code.
              REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF ',' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF 'DATA ' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF ':' IN wa_code WITH ''.

              REPLACE ALL OCCURRENCES OF 'OCCURS 0' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.


              CONDENSE wa_code.
              SPLIT wa_code AT space INTO lv_c11 lv_c12 lv_c13.
              CLEAR wa_code.

              CONCATENATE wa_code  lv_c11 'TYPE STANDARD TABLE OF'  lv_c13   ',' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
              INSERT  wa_code INTO it_code.
              IF sy-subrc = 0.
                wa_dip-count_no = lv_count + 1.
                wa_dip-obsolete = 'OCCURS'.
                wa_dip-zreplace =  'STANDARD TABLE OF..'.
                wa_dip-stfm = 'OCCURS IS REPLACED WITH STANDARD TABLE OF'.
                wa_dip-zprogram = ls_report-name.
                wa_dip-line = sy-tabix.
                APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                wa_stable-sno = wa_dip-count_no.
                wa_stable-objname = ls_report-name.
                wa_stable-processed = 'X'.
                INSERT /vshaneya/stable FROM wa_stable.
                CLEAR wa_stable.
*end of changes
              ENDIF.
            ENDIF.

            CLEAR wa_code.

          ELSEIF wa_code CS '><' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '><' IN wa_code WITH 'NE'.
            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = '><'.
              wa_dip-zreplace =  'NE'.
              wa_dip-stfm = '>< is replaced with NE'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes

            ENDIF.
            CLEAR wa_code.
**********************************************************************
***Begin Of Insert By Nanda->20.02.20
***For Addition
          ELSEIF wa_code+0(3) CS 'ADD' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            IF wa_code+0(3) = 'ADD'.
            REPLACE ALL OCCURRENCES OF 'ADD' IN wa_code WITH ' '.
            REPLACE ALL OCCURRENCES OF 'TO' IN wa_code WITH '+='.
            ENDIF.
*            REPLACE ALL OCCURRENCES OF 'ADD' IN wa_code WITH '+='.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'ADD'.
              wa_dip-zreplace =  '+='.
              wa_dip-stfm = 'ADD is replaced with +='.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
            ENDIF.
            CLEAR wa_code.

***For Subtraction
          ELSEIF wa_code CS 'SUBTRACT' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
*            REPLACE ALL OCCURRENCES OF 'SUBTRACT' IN wa_code WITH '-='.
            IF wa_code+0(8) = 'SUBTRACT'.
            REPLACE ALL OCCURRENCES OF 'SUBTRACT' IN wa_code WITH ' '.
            REPLACE ALL OCCURRENCES OF 'FROM' IN wa_code WITH '-='.
            ENDIF.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SUBTRACT'.
              wa_dip-zreplace =  '-='.
              wa_dip-stfm = 'SUBTRACT is replaced with -='.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
            ENDIF.
            CLEAR wa_code.

*** For Multiplication
          ELSEIF wa_code CS 'MULTIPLY' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            IF wa_code+0(8) = 'MULTIPLY'.
            REPLACE ALL OCCURRENCES OF 'MULTIPLY' IN wa_code WITH ' '.
            REPLACE ALL OCCURRENCES OF 'TO' IN wa_code WITH '*='.
            ENDIF.
*            REPLACE ALL OCCURRENCES OF 'MULTIPLY' IN wa_code WITH '*='.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'MULTIPLY'.
              wa_dip-zreplace =  '*='.
              wa_dip-stfm = 'MULTIPLY is replaced with *='.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
            ENDIF.
            CLEAR wa_code.

***For Divide
          ELSEIF wa_code CS 'DIVIDE' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
           IF wa_code+0(6) = 'DIVIDE'.
            REPLACE ALL OCCURRENCES OF 'DIVIDE' IN wa_code WITH ' '.
            REPLACE ALL OCCURRENCES OF 'TO' IN wa_code WITH '*='.
            ENDIF.
*            REPLACE ALL OCCURRENCES OF 'DIVIDE' IN wa_code WITH '/='.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'DIVIDE'.
              wa_dip-zreplace =  '/='.
              wa_dip-stfm = 'DIVIDE is replaced with /='.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
            ENDIF.
            CLEAR wa_code.
***End of Insert BY Nanda->20.02.20
**********************************************************************
          ELSEIF wa_code CS '=<' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '=<' IN wa_code WITH 'LE'.
            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = '=<'.
              wa_dip-zreplace =  'LE'.
              wa_dip-stfm = '=< is replaced with LE'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS '=<' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '=<' IN wa_code WITH 'LE'.
            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              lv_count = lv_count + 1.
              PERFORM update_log  USING lv_count '=<' '=<' '=< is replaced with LE' ls_report-name sy-tabix.
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'IS REQUESTED' AND  wa_code+0(1) NE '*'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF 'IS REQUESTED' IN wa_code WITH 'IS SUPPLIED'.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'IS REQUESTED'.
              wa_dip-zreplace =  ' IS SUPPLIED '.
              wa_dip-stfm = 'IS REQUESTED IS replaced with IS SUPPLIED'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'ASSIGN' AND wa_code NS 'LOCAL' AND wa_code+0(1) NE '*'..
            DATA lv_a1 TYPE string.
            DATA lv_a2 TYPE string.
            DATA lv_a3 TYPE string.
            DATA lv_a4 TYPE string.
            SPLIT wa_code AT '' INTO lv_a1 lv_a2 lv_a3 lv_a4.
            REPLACE ALL OCCURRENCES OF '(' IN lv_a2 WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN lv_a2 WITH ''.

          ELSEIF wa_code CS 'SORT'  AND wa_code+0(1) NE '*' AND wa_code CS 'BY <'.
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'SORT' ] ).
                CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
                MODIFY it_code FROM wa_code.
                REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.

                DATA lv_s1 TYPE string.
                DATA lv_s2 TYPE string.
                DATA lv_s3 TYPE string.
                DATA lv_s4 TYPE string.

                SPLIT wa_code AT '' INTO lv_s1 lv_s2 lv_s3 lv_s4.
                CONCATENATE lv_s1 lv_s2 lv_s3 lv_a2  '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
                INSERT wa_code INTO it_code.

                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'field symbols for dynamic component specifications'.
                  wa_dip-zreplace =  'parenthesized character'.
                  wa_dip-stfm = ''.
                  wa_dip-zprogram = ls_report-name.
                  wa_dip-line = sy-tabix.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.
                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code CS 'AT NEW <'  AND wa_code+0(1) NE '*' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
*            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            DATA lv_n1 TYPE string.
            DATA lv_n2 TYPE string.
            DATA lv_n3 TYPE string.
            SPLIT wa_code AT '' INTO lv_n1 lv_n2 lv_n3 .
            CONCATENATE lv_n1 lv_n2 lv_a2 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'components dynamically is obsolete'.
              wa_dip-zreplace =  ' replaced by (name). '.
              wa_dip-stfm = ''.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes

            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'SELECT SINGLE'  AND wa_code+0(1) NE '*' AND wa_code NS 'INTO' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
*            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE FIRST OCCURRENCE OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
*************************************************************************************************
*            DATA lv_ss1 TYPE string.
*            DATA lv_ss2 TYPE string.
*
*            DATA gv_select TYPE string.
*            SPLIT wa_code AT 'FROM' INTO lv_ss1 lv_ss2 .
*            CONCATENATE  lv_ss1+0(5) ' INTO '  lv_ss2+0(5) INTO gv_select.
*            REPLACE ALL OCCURRENCES OF lv_ss2+0(5) IN wa_code WITH  gv_select.

            DATA lv_ss1 TYPE string.
            DATA lv_ss2 TYPE string.
            DATA lv_var TYPE i.

            DATA gv_select TYPE string.
            clear: lv_ss1, lv_ss2, gv_select, lv_var.
            SPLIT wa_code AT 'FROM' INTO lv_ss1 lv_ss2.
            if  ( lv_ss1 is not INITIAL and lv_ss2 is not INITIAL ).
              clear lv_var.
            CONDENSE: lv_ss1, lv_ss2.
             lv_var = strlen( lv_ss2 ).
             if lv_var <= 4.
                CONCATENATE lv_ss2 ' ' ' ' into lv_ss2 SEPARATED BY space.
              ELSEIF lv_var <= 5.
                CONCATENATE lv_ss2 ' '  into lv_ss2 SEPARATED BY space.
              endif.
            CONCATENATE  lv_ss1+0(6) ' INTO '  lv_ss2+0(6) INTO gv_select SEPARATED BY space.
            REPLACE ALL OCCURRENCES OF lv_ss2+0(6) IN wa_code WITH  gv_select.

*************************************************************************************************
*************************************************************************************************

            CONCATENATE wa_code '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            endif.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SELECT SINGLE * dbtab'.
              wa_dip-zreplace =  ' SELECT SINGLE * dbtab into *dbtab '.
              wa_dip-line = sy-tabix.
              wa_dip-stfm = 'SELECT SINGLE * dbtab into *dbtab'.
              wa_dip-zprogram = ls_report-name.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'WAIT UNTIL'  AND wa_code+0(1) NE '*'  .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.

            REPLACE ALL OCCURRENCES OF 'WAIT' IN wa_code WITH 'WAIT FOR ASYNCHRONOUS TASKS'.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.

            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'WAIT UNTIL'.
              wa_dip-zreplace =  ' WAIT FOR ASYNCHRONOUS TASKS '.
              wa_dip-stfm = 'WAIT FOR ASYNCHRONOUS TASKS'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'INSERT'  AND wa_code+0(1) NE '*' AND wa_code NS 'FROM' AND wa_code+0(1) CS 'I' AND wa_code NS 'WHERE'
                                                                                            AND wa_code NS 'IF'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            DATA lv_in1 TYPE string.
            DATA lv_in2 TYPE string.
*
            SPLIT wa_code AT '' INTO lv_in1 lv_in2 .
            CONCATENATE lv_in1   lv_in2 'FROM ' lv_in2 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'INSERT dbtab'.
              wa_dip-zreplace =  'INSERT dbtab FROM dbtab '.
              wa_dip-stfm = 'INSERT dbtab FROM dbtab'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'UPDATE'  AND wa_code+0(1) NE '*' AND wa_code NS 'FROM' AND wa_code+0(1) CS 'U' AND wa_code NS 'WHERE'.
            DATA gv_stlen TYPE i.
            gv_stlen = strlen( wa_code ).
            IF gv_stlen GT 12.
              CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
              MODIFY it_code FROM wa_code.
              REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
              REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.

              CLEAR: lv_in1 , lv_in2.
              SPLIT wa_code AT '' INTO lv_in1 lv_in2 .
              CONCATENATE lv_in1   lv_in2 'FROM ' lv_in2 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
              INSERT wa_code INTO it_code.
              IF sy-subrc = 0.
                wa_dip-count_no = lv_count + 1.
                wa_dip-obsolete = 'UPDATE dbtab'.
                wa_dip-zreplace =  'UPDATE dbtab FROM dbtab '.
                wa_dip-stfm = 'UPDATE dbtab FROM dbtab '.
                wa_dip-zprogram = ls_report-name.
                wa_dip-line = sy-tabix.
                APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                wa_stable-sno = wa_dip-count_no.
                wa_stable-objname = ls_report-name.
                wa_stable-processed = 'X'.
                INSERT /vshaneya/stable FROM wa_stable.
                CLEAR wa_stable.
*end of changes
              ENDIF.
              CLEAR wa_code.
            ENDIF.
          ELSEIF wa_code CS 'MODIFY'  AND wa_code+0(1) NE '*' AND wa_code NS 'FROM' AND wa_code+0(1) CS 'M' AND wa_code NS 'WHERE'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.

            CLEAR: lv_in1 , lv_in2.
            SPLIT wa_code AT '' INTO lv_in1 lv_in2 .
            CONCATENATE lv_in1   lv_in2 'FROM ' lv_in2 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'MODIFY dbtab'.
              wa_dip-zreplace =  'MODIFY dbtab FROM dbtab '.
              wa_dip-stfm = 'MODIFY dbtab FROM dbtab'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'DELETE'  AND wa_code+0(1) NE '*' AND wa_code NS 'FROM' AND wa_code+0(1) CS 'D' AND wa_code NS 'WHERE'.
            IF wa_code NS 'DATASET' AND wa_code NS 'DYNPRO' AND wa_code NS 'REPORT' AND Wa_code NS 'TEXTPOOL' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.

            CLEAR: lv_in1 , lv_in2.
            SPLIT wa_code AT '' INTO lv_in1 lv_in2 .
            CONCATENATE lv_in1   lv_in2 'FROM ' lv_in2 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'DELETE dbtab'.
              wa_dip-zreplace =  'DELETE dbtab FROM dbtab '.
              wa_dip-line = sy-tabix.
              wa_dip-stfm = 'DELETE dbtab FROM dbtab'.
              wa_dip-zprogram = ls_report-name.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

            ENDIF.
          ELSEIF wa_code CS 'SELECT-OPTIONS'  AND wa_code+0(1) NE '*' AND wa_code CS 'NO DATABASE SELECTION' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF 'NO DATABASE SELECTION' IN wa_code WITH ' '.

            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SELECT-OPTIONS - NO DATABASE SELECTION '.
              wa_dip-zreplace =  'SELECT-OPTIONS'.
              wa_dip-stfm = 'SELECT-OPTIONS'.
              wa_dip-line = sy-tabix.
              wa_dip-zprogram = ls_report-name.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'ASSIGN LOCAL COPY OF'  AND wa_code+0(1) NE '*' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            DATA lv_c1 TYPE string.
            DATA lv_c2 TYPE string.
            DATA lv_c3 TYPE string.
            DATA lv_c4 TYPE string.
            DATA lv_c5 TYPE string.
            DATA lv_c6 TYPE string.
            DATA lv_c7 TYPE string.
            SPLIT wa_code AT '' INTO lv_c1 lv_c2 lv_c3 lv_c4 lv_c5 lv_c6 lv_c7.
            CONCATENATE  'DATA dref TYPE REF TO data.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            CONCATENATE 'CREATE DATA dref LIKE ' lv_c5 '.' '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            CONCATENATE 'ASSIGN dref->* TO ' lv_c7 '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'ASSIGN LOCAL COPY'.
              wa_dip-zreplace =  ' Replaced with data reference '.
              wa_dip-stfm = 'Replaced with data reference'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'LOOP AT SCREEN '  AND wa_code+0(1) NE '*' AND wa_code NS 'LOOP AT SCREEN INTO SCREEN ' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.

            REPLACE ALL OCCURRENCES OF 'LOOP AT SCREEN ' IN wa_code WITH 'LOOP AT SCREEN INTO SCREEN. '.
            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'LOOP AT SCREEN '.
              wa_dip-zreplace =  'LOOP AT SCREEN INTO SCREEN '.
              wa_dip-stfm = 'LOOP AT SCREEN TO SCREEN is Replaced'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'MODIFY SCREEN ' AND wa_code+0(1) NE '*' AND  wa_code NS 'FROM SCREEN '.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.

            REPLACE ALL OCCURRENCES OF 'MODIFY SCREEN ' IN wa_code WITH 'MODIFY SCREEN FROM SCREEN. '.
            INSERT wa_code INTO  it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'MODIFY SCREEN '.
              wa_dip-zreplace =  'MODIFY SCREEN FROM SCREEN'.
              wa_dip-stfm = 'MODIFY SCREEN FROM SCREEN is Replaced'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'MOVE' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'M'.

            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF 'MOVE' IN wa_code WITH '*MOVE'.
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*MOVE:' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*MOVE :' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*MOVE' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool'  IN wa_code WITH ''.
            SPLIT wa_code AT 'TO' INTO gv_move1  gv_move2.

            CONCATENATE gv_move2 '=' gv_move1 '. "Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            CONDENSE wa_code.
            gv_tabix = sy-tabix + 1.
            INSERT  wa_code INTO  it_code INDEX gv_tabix.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'MOVE'.
              wa_dip-zreplace =  '= '.
              wa_dip-stfm = 'Replaced with ='.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'PERFORM' AND wa_code+0(1) NE '*' AND wa_code CS '(' AND wa_code CS ')'.

            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF ')' IN wa_code WITH ''.
            SPLIT wa_code AT '(' INTO lv_ps1  lv_ps2.

            CONCATENATE lv_ps1 'IN PROGRAM'  lv_ps2 '"Added by Haneya Tool' INTO wa_code SEPARATED BY space.
            CONDENSE wa_code.
            gv_tabix = sy-tabix + 1.
            INSERT  wa_code INTO  it_code INDEX gv_tabix.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'PERFORM EXTERNAL'.
              wa_dip-zreplace =  'IN ADDITION PROGRAM NAME'.
              wa_dip-stfm = 'IN ADDITION PROGRAM NAME'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'PARAMETER' AND wa_code+0(1) NE '*' AND wa_code+1(9) NS 'S' AND wa_code+0(1) CS 'P' AND wa_code NS '='.

            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            REPLACE ALL OCCURRENCES OF 'PARAMETER' IN wa_code WITH 'PARAMETERS'.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            CONDENSE wa_code.
            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'PARAMETER'.
              wa_dip-zreplace =  'PARAMETERS '.
              wa_dip-stfm = 'PARAMETER is replaced with PARAMETERS'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'PACK' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'P'.
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'PACK' ] ).
                REPLACE ALL OCCURRENCES OF 'PACK' IN wa_code WITH '*PACK'.
                MODIFY it_code FROM wa_code.
                REPLACE ALL OCCURRENCES OF '*PACK' IN wa_code WITH ''.
                REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
                SPLIT wa_code AT 'TO' INTO gv_pack1  gv_pack2.

                CONCATENATE gv_pack2 '=' gv_pack1 '.' INTO wa_code SEPARATED BY space.
                CONDENSE wa_code.
                gv_ptabix = sy-tabix + 1.
                INSERT  wa_code INTO  it_code INDEX gv_ptabix.
                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'PACK'.
                  wa_dip-zreplace =  '= '.
                  wa_dip-stfm = '= Replaced'.
                  wa_dip-line = sy-tabix.
                  wa_dip-zprogram = ls_report-name.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.
                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code CS 'UNICODE ENABLING '  AND wa_code+0(1) NE '*' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF 'UNICODE ENABLING ' IN wa_code WITH 'VERSION '.
            INSERT wa_code INTO it_code .
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'UNICODE ENABLING'.
              wa_dip-zreplace =  'VERSION '.
              wa_dip-stfm = 'VERSION Replaced'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF  wa_code CS 'SET EXTENDED CHECK OFF'  AND wa_code+0(1) NE '*' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SET EXTENDED CHECK OFF'.
              wa_dip-zreplace =  'SPACE '.
              wa_dip-stfm = 'SET EXTENDED CHECK OFF Commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'SET EXTENDED CHECK ON'  AND wa_code+0(1) NE '*' .
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SET EXTENDED CHECK ON'.
              wa_dip-zreplace =  'SPACE '.
              wa_dip-stfm = 'SET EXTENDED CHECK ON Commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'TYPE-POOLS' AND wa_code+0(1) NE '*'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'TYPE-POOLS'.
              wa_dip-zreplace =  'Commented'.
              wa_dip-stfm = 'Type Pools commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'DEFINITION' AND wa_code+0(1) NE '*'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'CLASS DEFINITION LOAD'.
              wa_dip-zreplace =  'Commented'.
              wa_dip-stfm = 'CLASS DEFINITION LOAD commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'INTERFACE' AND wa_code+0(1) NE '*'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'INTERFACE LOAD'.
              wa_dip-zreplace =  'Commented'.
              wa_dip-stfm = 'INTERFACE LOAD commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'FIELDS' AND wa_code+0(1) NE '*' AND wa_code+0(1) EQ 'F' .
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'FIELDS' ] ).
                IF wa_code CS '##NEEDED' OR wa_code CS 'FIELDS ='.
                  CONTINUE.
                ENDIF.
                REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
                CONCATENATE wa_code '##NEEDED.' INTO wa_code SEPARATED BY space.
                MODIFY it_code FROM wa_code.
                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'Data Object'.
                  wa_dip-zreplace =  '##NEEDED.'.
                  wa_dip-stfm = 'Psedo Code  added for the Data object'.
                  wa_dip-zprogram = ls_report-name.
                  wa_dip-line = sy-tabix.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.

                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code CS 'TABLE LINE' .
            REPLACE ALL OCCURRENCES OF 'TABLE LINE' IN wa_code WITH 'TABLE_LINE'.
            CONCATENATE wa_code '"Changed by Haneya Tool' INTO wa_code.
            MODIFY it_code FROM wa_code.
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'TABLE LINE'.
              wa_dip-zreplace =  'TABLE_LINE'.
              wa_dip-stfm = 'TABLE LINE is replaced with TABLE_LINE'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'SET LOCALE' AND wa_code+0(1) NE '*'  AND wa_code NS '"COUNTRY'.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF 'COUNTRY' IN wa_code WITH '."COUNTRY'.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.

            INSERT wa_code INTO it_code.

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'Set Locale COUNTRY'.
              wa_dip-zreplace =  'in Addition COUNTRY Commented'.
              wa_dip-stfm = 'COUNTRY Commented'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
*          ELSEIF WA_CODE CS 'GET LOCALE' AND WA_CODE+0(1) NE '*' AND WA_CODE NS '"COUNTRY'.
*
*            CONCATENATE '*' WA_CODE '"Commented by Haneya Tool' INTO WA_CODE .
*            MODIFY IT_CODE FROM WA_CODE.
*            REPLACE ALL OCCURRENCES OF 'COUNTRY' IN WA_CODE WITH '."COUNTRY'.
*            REPLACE ALL OCCURRENCES OF 'Commented' IN WA_CODE WITH 'Added'.
*            REPLACE ALL OCCURRENCES OF '*' IN WA_CODE WITH ''.
*
*            INSERT WA_CODE INTO IT_CODE.
*            IF SY-SUBRC = 0.
*              WA_DIP-COUNT_NO = LV_COUNT + 1.
*              WA_DIP-OBSOLETE = ' Get Locale COUNTRY'.
*              WA_DIP-ZREPLACE =  'in Addition COUNTRY Commented'.
*              WA_DIP-STFM = 'COUNTRY Commented'.
*              WA_DIP-ZPROGRAM = LS_REPORT-NAME.
*              WA_DIP-LINE = SY-TABIX.
*              APPEND WA_DIP TO IT_DIP.
*            ENDIF.
*            CLEAR WA_CODE.
          ELSEIF wa_code CS 'CALL METHOD' AND wa_code CS '=>' AND wa_code+0(1) NE '*'..
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF 'CALL METHOD' IN wa_code WITH ''.

            INSERT wa_code INTO it_code .
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'CALL METHOD'.
              wa_dip-zreplace =  'Removed Call Method'.
              wa_dip-stfm = 'STATIC METHOD SYNTAX REPLACED'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'REFRESH' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'R' AND wa_code CS 'REFRESH :'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'REFRESH' IN wa_code WITH 'CLEAR'.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN wa_code WITH ''.
            CONCATENATE wa_code '[].' '"Added by Haneya Tool' INTO wa_code.
            INSERT wa_code INTO it_code .
            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'REFRESH'.
              wa_dip-zreplace =  'CLEAR'.
              wa_dip-stfm = 'REFRESH is replaced with CLEAR'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
*              INSERT /vshaneya/zprg FROM wa_dip.
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'ADD-CORRESPONDING' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'A'.

            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN  wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'ADD-CORRESPONDING' IN wa_code WITH ''.
*            SPLIT wa_code AT 'TO' INTO DATA(l1) DATA(l2).
*            DATA(grs_desc) = cl_abap_structdescr=>describe_by_data( l1 ).
*            DATA(grd_desc) = cl_abap_structdescr=>describe_by_data( l2 ).
*
*            grs_sdesc ?= grs_desc.
*            grd_Sdesc ?= grd_desc.
*            LOOP AT grs_sdesc->components INTO gss_comp.
*
*
*              READ TABLE  grs_sdesc->components INTO gsd_comp INDEX sy-tabix.
*              IF sy-subrc EQ 0.
*                CONCATENATE 'ADD' gss_comp-name  'TO' gsd_comp-name INTO wa_code.
*                INSERT wa_code INTO it_code.
*              ENDIF.
*            ENDLOOP.
*            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'ADD-CORRESPONDING'.
              wa_dip-zreplace =  'ADD'.
              wa_dip-stfm = 'ADD-CORRESPONDING is replaced with ADD'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'SUBTRACT-CORRESPONDING' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'S'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '-CORRESPONDING' IN wa_code WITH ''.

            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SUBTRACT-CORRESPONDING'.
              wa_dip-zreplace =  'SUBTRACT'.
              wa_dip-stfm = 'SUBTRACT-CORRESPONDING is replaced with SUBTRACT'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
*              INSERT /vshaneya/zprg FROM wa_dip.
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'MULTIPLY-CORRESPONDING' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'M'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '-CORRESPONDING' IN wa_code WITH ''.
            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'MULTIPLY-CORRESPONDING'.
              wa_dip-zreplace =  'MULTIPLY'.
              wa_dip-stfm = 'MULTIPLY-CORRESPONDING is replaced with MULTIPLY'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'DIVIDE-CORRESPONDING' AND wa_code+0(1) NE '*' AND wa_code+0(1) CS 'D'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
            REPLACE ALL OCCURRENCES OF '-CORRESPONDING' IN wa_code WITH ''.

            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'DIVIDE-CORRESPONDING'.
              wa_dip-zreplace =  'DIVIDE'.
              wa_dip-stfm = 'DIVIDE-CORRESPONDING is replaced with DIVIDE'.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'DETAIL' AND wa_code+0(1) NE '*' AND wa_code+0(1) EQ 'D' AND wa_code NS 'DATA DETAIL'.
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'DETAIL' ] ).
                CONDENSE wa_code.
                CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
                MODIFY it_code FROM wa_code.
                REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
                REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
                REPLACE ALL OCCURRENCES OF 'DETAIL' IN wa_code WITH 'FORMAT INTENSIFIED OFF '.

                INSERT wa_code INTO it_code .

                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'DETAIL'.
                  wa_dip-zreplace =  'FORMAT INTENSIFIED OFF '.
                  wa_dip-stfm = 'DETAIL is replaced with FORMAT INTENSIFIED OFF. '.
                  wa_dip-zprogram = ls_report-name.
                  wa_dip-line = sy-tabix.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.
                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code CS 'SUMMARY' AND wa_code+0(1) NE '*'.
            CONDENSE wa_code.
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'SUMMARY' ] ).
                CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
                MODIFY it_code FROM wa_code.
                REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
                REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
                REPLACE ALL OCCURRENCES OF 'SUMMARY' IN wa_code WITH 'FORMAT INTENSIFIED ON '.

                INSERT wa_code INTO it_code .

                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'SUMMARY'.
                  wa_dip-zreplace =  'FORMAT INTENSIFIED ON '.
                  wa_dip-stfm = 'SUMMARY is replaced with FORMAT INTENSIFIED ON. '.
                  wa_dip-zprogram = ls_report-name.
                  wa_dip-line = sy-tabix.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.
                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code  CS 'INPUT.' AND wa_code+0(1) NE '*' .
            CONDENSE wa_code.
            FREE: lv_code1, lv_code2.
            SPLIT wa_code AT '"' INTO lv_code1 lv_code2.
            IF lv_code1 IS NOT INITIAL .
              REPLACE ALL OCCURRENCES OF '.' IN lv_code1 WITH ''.
              SPLIT lv_code1 AT space INTO TABLE itab.
              IF line_exists( itab[ code_word = 'INPUT' ] ).
                CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
                MODIFY it_code FROM wa_code.
                REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
                REPLACE ALL OCCURRENCES OF 'Commented' IN wa_code WITH 'Added'.
                REPLACE ALL OCCURRENCES OF 'INPUT' IN wa_code WITH 'FORMAT INPUT ON '.

                INSERT wa_code INTO it_code .

                IF sy-subrc = 0.
                  wa_dip-count_no = lv_count + 1.
                  wa_dip-obsolete = 'INPUT'.
                  wa_dip-zreplace =  'FORMAT INPUT ON '.
                  wa_dip-stfm = 'INPUT is replaced with FORMAT INPUT ON. '.
                  wa_dip-zprogram = ls_report-name.
                  wa_dip-line = sy-tabix.
                  APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
                  wa_stable-sno = wa_dip-count_no.
                  wa_stable-objname = ls_report-name.
                  wa_stable-processed = 'X'.
                  INSERT /vshaneya/stable FROM wa_stable.
                  CLEAR wa_stable.
*end of changes
                ENDIF.
                CLEAR wa_code.
              ENDIF.
            ENDIF.
          ELSEIF wa_code CS 'REPLACE' AND wa_code CS 'INTO' AND  wa_code+0(1) NE '*'.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.
            DATA :gv_r1 TYPE string,
                  gv_r2 TYPE string,
                  gv_r3 TYPE string,
                  gv_r4 TYPE string,
                  gv_r5 TYPE string,
                  gv_r6 TYPE string.
            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            SPLIT wa_code AT ' ' INTO gv_r1 gv_r2 gv_r3 gv_r4 gv_r5 gv_r6.
            CONCATENATE 'REPLACE ALL OCCURRENCES OF' gv_r2 'IN' gv_r6 'WITH' gv_r4  '.' '"Added by Haneya Tool'  INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'REPLACE'.
              wa_dip-zreplace =  'REPLACE ALL OCCURENCES '.
              wa_dip-stfm = 'REPLACE is replaced with REPLACE ALL OCCURENCES. '.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.

          ELSEIF wa_code CS 'SEARCH' AND wa_code+0(1) NE '*' AND wa_code NS 'BINARY' AND wa_code NS 'WRITE'.
            CONDENSE wa_code.
            DATA wa_code2 TYPE string.

            wa_code2 = 'DATA GS_result_tab TYPE match_result_tab. "Added by Haneya Tool'.
            INSERT wa_code2 INTO it_code INDEX sy-tabix..

            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code .
            MODIFY it_code FROM wa_code.

            REPLACE ALL OCCURRENCES OF '"Commented by Haneya Tool' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '*' IN wa_code WITH ''.
            REPLACE ALL OCCURRENCES OF '.' IN wa_code WITH ''.
            SPLIT wa_code AT ' ' INTO gv_f1 gv_f2 gv_f3 gv_f4 .

            CONCATENATE 'FIND ALL OCCURRENCES OF' gv_f4 'IN' gv_f2 'RESULTS GS_result_tab'  '.' '"Added by Haneya Tool'  INTO wa_code SEPARATED BY space.
            INSERT wa_code INTO it_code .

            IF sy-subrc = 0.
              wa_dip-count_no = lv_count + 1.
              wa_dip-obsolete = 'SEARCH'.
              wa_dip-zreplace =  'FIND ALL OCCURENCES '.
              wa_dip-stfm = 'SEARCH is replaced with FIND ALL OCCURENCES. '.
              wa_dip-zprogram = ls_report-name.
              wa_dip-line = sy-tabix.
              APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
              wa_stable-sno = wa_dip-count_no.
              wa_stable-objname = ls_report-name.
              wa_stable-processed = 'X'.
              INSERT /vshaneya/stable FROM wa_stable.
              CLEAR wa_stable.
*end of changes
            ENDIF.
            CLEAR wa_code.
          ELSEIF wa_code CS 'CALL FUNCTION ''/ISDFPS/BAPI_BATCH_SAVE_REPLIC''' AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = '/ISDFPS/BAPI_BATCH_SAVE_REPLIC'.
            wa_dip-zreplace =  'BAPI_BATCH_SAVE_REPLICA'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CLEAR wa_dip.

            CONDENSE wa_code.
            CONCATENATE '*' wa_code  INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING lv_tab1 '/ISDFPS/BAPI_BATCH_SAVE_REPLIC' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.

            PERFORM replace_fnmodule USING 'BAPI_BATCH_SAVE_REPLICA'  l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''BAPI_INSPCHAR_GETRESULT'''
            OR wa_code CS 'CALL FUNCTION ''BAPI_INSPCHAR_GETREQUIREMENTS'''
            OR wa_code CS 'CALL FUNCTION ''BAPI_INSPPOINT_GETREQUIREMENTS'''
            OR wa_code CS 'CALL FUNCTION ''BAPI_INSPPOINT_GETLIST''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'BAPI_INSPCHAR_GETREQUIREMENTS'.
              wa_dip-obsolete = 'BAPI_INSPCHAR_GETREQUIREMENTS'.
            ENDIF.
            IF wa_code CS 'BAPI_INSPCHAR_GETRESULT'.
              wa_dip-obsolete = 'BAPI_INSPCHAR_GETRESULT'.
            ENDIF.
            IF wa_code CS 'BAPI_INSPPOINT_GETREQUIREMENTS'.
              wa_dip-obsolete = 'BAPI_INSPPOINT_GETREQUIREMENTS'.
            ENDIF.
            IF wa_code CS 'BAPI_INSPPOINT_GETLIST'.
              wa_dip-obsolete = 'BAPI_INSPPOINT_GETLIST'.
            ENDIF.
            wa_dip-zreplace =  'BAPI_INSPOPER_GETDETAIL'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes

            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix 'BAPI_INSPCHAR_GETREQUIREMENTS' wa_code CHANGING param param2 param3 param4 param5 param6.
            PERFORM replace_fnmodule USING 'BAPI_INSPOPER_GETDETAIL' l_tabix.
*            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''CMS_LO_ANALYZE_DATA_IL''') AND wa_code+0(1) NE '*'.
            REPLACE ALL OCCURRENCES OF 'CMS_LO_ANALYZE_DATA_IL' IN wa_code WITH 'CMS_LO_ANALYZE_DATA_IL_1'.

            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.

            IF wa_code CS 'CMS_LO_ANALYZE_DATA_IL'.
              wa_dip-obsolete = 'CMS_LO_ANALYZE_DATA_IL'.
            ENDIF.

            wa_dip-zreplace =  'CMS_LO_ANALYZE_DATA_IL_1'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes

            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.

          ELSEIF ( wa_code CS 'CALL FUNCTION ''COM_PRODUCT_GETLIST''') AND wa_code+0(1) NE '*'.
            REPLACE ALL OCCURRENCES OF 'COM_PRODUCT_GETLIST' IN wa_code WITH 'COM_PRODUCT_GETLIST2'.

            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.

            IF wa_code CS 'COM_PRODUCT_GETLIST'.
              wa_dip-obsolete = 'COM_PRODUCT_GETLIST'.
            ENDIF.

            wa_dip-zreplace =  'COM_PRODUCT_GETLIST2'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes

            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.

          ELSEIF ( wa_code CS 'CALL FUNCTION ''BAPI_INSPCHAR_SETRESULT'''
                   OR wa_code CS 'CALL FUNCTION ''BAPI_INSPPOINT_CHANGE'''
                   OR wa_code CS 'CALL FUNCTION ''BAPI_INSPPOINT_CREATEFROMDATA''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'BAPI_INSPCHAR_SETRESULT'.
              wa_dip-obsolete = 'BAPI_INSPCHAR_SETRESULT'.
            ENDIF.
            IF wa_code CS 'BAPI_INSPPOINT_CHANGE'.
              wa_dip-obsolete = 'BAPI_INSPPOINT_CHANGE'.
            ENDIF.

            IF wa_code CS 'BAPI_INSPPOINT_CREATEFROMDATA'.
              wa_dip-obsolete = 'BAPI_INSPPOINT_CREATEFROMDATA'.
            ENDIF.

            wa_dip-zreplace =  'BAPI_INSPOPER_RECORDRESULTS'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            IF wa_code CS 'BAPI_INSPCHAR_SETRESULT'.
              PERFORM get_fm_parameters USING l_tabix 'BAPI_INSPCHAR_SETRESULT' wa_code CHANGING param param2 param3 param4 param5 param6.
            ELSE.
              PERFORM get_fm_parameters USING l_tabix 'BAPI_INSPPOINT_CHANGE' wa_code CHANGING param param2 param3 param4 param5 param6.
            ENDIF.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_INSPOPER_RECORDRESULTS' l_tabix..
*            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''BAPI_XBP_JOB_SPOOLLIST_READ'''
                 OR wa_code CS 'CALL FUNCTION ''BAPI_XBP_JOB_SPOOLLST_READ_RW''')
                 AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'BAPI_XBP_JOB_SPOOLLST_READ_RW'.
              wa_dip-obsolete = 'BAPI_XBP_JOB_SPOOLLST_READ_RW'.
            ENDIF.
            IF wa_code CS 'BAPI_XBP_JOB_SPOOLLIST_READ'.
              wa_dip-obsolete = 'BAPI_XBP_JOB_SPOOLLIST_READ'.
            ENDIF.
            wa_dip-zreplace =  'BAPI_XBP_JOB_SPOOLLIST_READ_20'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BAPI_XBP_JOB_SPOOLLIST_READ' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_XBP_JOB_SPOOLLIST_READ_20' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_DISPLAY''' OR wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_DISPLAY''')
             AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'ASH_PM_QMEL_DISPLAY'.
              wa_dip-obsolete = 'ASH_PM_QMEL_DISPLAY'.
            ENDIF.
            IF wa_code CS 'ASH_SM_QMEL_DISPLAY'.
              wa_dip-obsolete = 'ASH_SM_QMEL_DISPLAY'.
            ENDIF.
            wa_dip-zreplace =  'ASH_QMEL_DISPLAY'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            l_tabix = sy-tabix.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            IF wa_code CS 'ASH_SM_QMEL_DISPLAY'.
              PERFORM get_fm_parameters USING l_tabix 'ASH_PM_QMEL_DISPLAY' wa_code CHANGING param param2 param3 param4 param5 param6.

            ELSE.
              PERFORM get_fm_parameters USING l_tabix 'ASH_SM_QMEL_DISPLAY' wa_code CHANGING param param2 param3 param4 param5 param6.

            ENDIF.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'ASH_QMEL_DISPLAY' l_tabix.
            CONTINUE.

          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_ORIGIN_GET''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'ASH_PM_QMEL_ORIGIN_GET'.
            wa_dip-zreplace =  'ASH_QMEL_ORIGIN_GET'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix 'ASH_PM_QMEL_ORIGIN_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'ASH_QMEL_ORIGIN_GET' l_tabix.
*            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_READ''' OR wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_READ''' ) AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'ASH_PM_QMEL_READ'.
              wa_dip-obsolete = 'ASH_PM_QMEL_READ'.
            ENDIF.
            IF wa_code CS 'ASH_SM_QMEL_READ'.
              wa_dip-obsolete = 'ASH_SM_QMEL_READ'.
            ENDIF.
            wa_dip-zreplace =  'ASH_QMEL_READ'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix 'ASH_PM_QMEL_READ' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'ASH_QMEL_READ' l_tabix.
*            CONTINUE.
*          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_READ''') AND wa_code+0(1) NE '*'.
*            l_tabix = sy-tabix.
*            wa_dip-count_no = lv_count + 1.
*            wa_dip-obsolete = 'ASH_PM_QMEL_READ'.
*            wa_dip-zreplace =  'ASH_QMEL_READ'.
*            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
*            wa_dip-zprogram = ls_report-name.
*            APPEND wa_dip TO it_dip.
*
*            CLEAR wa_dip.
*            CONDENSE wa_code.
*            CONCATENATE '*' wa_code INTO wa_code.
*            CONDENSE wa_code.
*            MODIFY it_code FROM wa_code.
*            CLEAR wa_code.
*            PERFORM replace_fnmodule USING 'ASH_QMEL_READ' l_tabix.
*            CONTINUE.

          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_ORIGIN_GET''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'ASH_SM_QMEL_ORIGIN_GET'.
            wa_dip-zreplace =  'ASH_QMEL_ORIGIN_GET'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_stable-sno = wa_dip-count_no.
            wa_stable-objname = ls_report-name.
            wa_stable-processed = 'X'.
            INSERT /vshaneya/stable FROM wa_stable.
            CLEAR wa_stable.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix 'ASH_SM_QMEL_ORIGIN_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'ASH_QMEL_ORIGIN_GET' l_tabix.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''WS_DOWNLOAD''') AND wa_code+0(1) NE '*' ) OR ( ( wa_code CS 'CALL FUNCTION ''DOWNLOAD''') AND wa_code+0(1) NE '*' ) ) .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'WS_DOWNLOAD'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'GUI_DOWNLOAD'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

*            PERFORM get_fm_parameters USING lv_tab1 'WS_DOWNLOAD' wa_code CHANGING param param2 param3 param4 param5 param6.
            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
*            PERFORM replace_fnmodule USING 'GUI_DOWNLOAD' l_tabix.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'GUI_DOWNLOAD' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
***Changes for Tech remediation - Avi
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''CLPB_EXPORT''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'CLPB_EXPORT'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'CLIPBOARD_EXPORT'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'CLIPBOARD_EXPORT' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''CLPB_IMPORT''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'CLPB_IMPORT'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_IMPORT'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'CLIPBOARD_IMPORT'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'CLIPBOARD_IMPORT' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''WS_UPLOAD''') AND wa_code+0(1) NE '*' ) OR ( ( wa_code CS 'CALL FUNCTION ''UPLOAD''') AND wa_code+0(1) NE '*' ) ) .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'WS_UPLOAD'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'GUI_UPLOAD'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'GUI_UPLOAD' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''CLPB_EXPORT''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'CLPB_EXPORT'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'CLIPBOARD_EXPORT'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'CLIPBOARD_EXPORT' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''REGISTRY_GET''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'REGISTRY_GET'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>REGISTRY_GET_VALUE'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'REGISTRY_GET_VALUE'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'REGISTRY_GET_VALUE' it_param it_exce it_seosubcodf l_tabix it_old_par.
*            CONTINUE.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''WS_FILE_ATTRIB''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'WS_FILE_ATTRIB'.
            wa_dip-zreplace =  'CL_GUI_FRONTEND_SERVICES=>FILE_GET_ATTRIBUTES'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_GUI_FRONTEND_SERVICES'
                  im_v_method      = 'FILE_GET_ATTRIBUTES'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class USING 'CL_GUI_FRONTEND_SERVICES' 'FILE_GET_ATTRIBUTES' it_param it_exce it_seosubcodf l_tabix it_old_par.
***class cl_icl_iif_ps
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''ICL_PER_TICL003_SELECT''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'ICL_PER_TICL003_SELECT'.
            wa_dip-zreplace =  'CL_ICL_IIF_PS=>ICL_PER_TICL003_SELECT'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_ICL_IIF_PS'
                  im_v_method      = 'ICL_PER_TICL003_SELECT'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_icl USING 'CL_ICL_IIF_PS' 'ICL_PER_TICL003_SELECT' it_param it_exce it_seosubcodf l_tabix it_old_par.
***CL_OO_INCLUDE_NAMING
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_CP_NAME''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'SEO_CLASS_GET_CP_NAME'.
            wa_dip-zreplace =  'CL_OO_INCLUDE_NAMING=>GET_INCLUDE_BY_TRKEY'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.

            CLEAR wa_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_OO_INCLUDE_NAMING'
                  im_v_method      = 'GET_INCLUDE_BY_TRKEY'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_oo USING 'CL_OO_INCLUDE_NAMING' 'GET_INCLUDE_BY_TRKEY' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_EXT_BY_INCLUDE''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'SEO_CLASS_GET_EXT_BY_INCLUDE'.
            wa_dip-zreplace =  'CL_OO_INCLUDE_NAMING=>GET_EXTENSION_OF_INCLUDE'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_OO_INCLUDE_NAMING'
                  im_v_method      = 'GET_EXTENSION_OF_INCLUDE'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_oo USING 'CL_OO_INCLUDE_NAMING' 'GET_EXTENSION_OF_INCLUDE' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_METHOD_INCLUDES'''
            OR wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_NAME_BY_INCLUDE'''
            OR wa_code CS 'CALL FUNCTION ''SEO_INTERFACE_GET_NAME_BY_INCL'''
            OR wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_CL_NAME''' ) AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_METHOD_INCLUDES'''.
              wa_dip-obsolete = 'SEO_CLASS_GET_METHOD_INCLUDES'.
            ELSEIF wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_NAME_BY_INCLUDE'''.
              wa_dip-obsolete = 'SEO_CLASS_GET_NAME_BY_INCLUDE'.
            ELSEIF wa_code CS 'CALL FUNCTION ''SEO_INTERFACE_GET_NAME_BY_INCL'''.
              wa_dip-obsolete = 'SEO_INTERFACE_GET_NAME_BY_INCL'.
            ELSEIF wa_code CS 'CALL FUNCTION ''SEO_CLASS_GET_CL_NAME'''.
              wa_dip-obsolete = 'SEO_CLASS_GET_CL_NAME'.
            ENDIF.
            wa_dip-zreplace =  'CL_OO_INCLUDE_NAMING=>GET_INSTANCE_BY_NAME'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_OO_INCLUDE_NAMING'
                  im_v_method      = 'GET_INSTANCE_BY_NAME'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_oo USING 'CL_OO_INCLUDE_NAMING' 'GET_INSTANCE_BY_NAME' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''SEO_METHOD_GET_INCLUDE_BY_NAME''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'SEO_METHOD_GET_INCLUDE_BY_NAME'.
            wa_dip-zreplace =  'CL_OO_INCLUDE_NAMING=>IF_OO_CLASS_INCL_NAMING~GET_INCLUDE_BY_MTDNAME'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_OO_INCLUDE_NAMING'
                  im_v_method      = 'IF_OO_CLASS_INCL_NAMING~GET_INCLUDE_BY_MTDNAME'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            IF it_seosubcodf IS INITIAL.
              SELECT * FROM seosubcodf
                INTO TABLE it_seosubcodf
                WHERE clsname = 'IF_OO_CLASS_INCL_NAMING'
                  AND cmpname = 'GET_INCLUDE_BY_MTDNAME'.
            ENDIF.
            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_oo USING 'IF_OO_CLASS_INCL_NAMING' 'GET_INCLUDE_BY_MTDNAME' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''SEO_METHOD_GET_NAME_BY_INCLUDE''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'SEO_METHOD_GET_NAME_BY_INCLUDE'.
            wa_dip-zreplace =  'CL_OO_INCLUDE_NAMING=>IF_OO_CLASS_INCL_NAMING~GET_MTDNAME_BY_INCLUDE'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_table.
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_OO_INCLUDE_NAMING'
                  im_v_method      = 'IF_OO_CLASS_INCL_NAMING~GET_MTDNAME_BY_INCLUDE'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            IF it_seosubcodf IS INITIAL.
              SELECT * FROM seosubcodf
                INTO TABLE it_seosubcodf
                WHERE clsname = 'IF_OO_CLASS_INCL_NAMING'
                  AND cmpname = 'GET_MTDNAME_BY_INCLUDE'.
            ENDIF.
            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_oo USING 'IF_OO_CLASS_INCL_NAMING' 'GET_MTDNAME_BY_INCLUDE' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF  ( ( wa_code CS 'CALL FUNCTION ''ICL_EVALUATE_REQUEST''') AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'ICL_EVALUATE_REQUEST'.
            wa_dip-zreplace =  'CL_ICL_BRF_PLUS_SERVICES=>ICL_EVALUATE_EXPRESSION'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes

            CLEAR wa_table1.
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_ICL_BRF_PLUS_SERVICES'
                  im_v_method      = 'ICL_EVALUATE_EXPRESSION'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_brf USING 'CL_ICL_BRF_PLUS_SERVICES' 'ICL_EVALUATE_EXPRESSION' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( ( wa_code CS 'CALL FUNCTION ''ICLE_USCNTXT_UPDATE''') OR wa_code CS 'CALL FUNCTION ''ICL_USCNTXT_UPDATE''' ) AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''ICLE_USCNTXT_UPDATE'''.
              wa_dip-obsolete = 'ICLE_USCNTXT_UPDATE'.
            ELSEIF wa_code CS 'CALL FUNCTION ''ICL_USCNTXT_UPDATE'''.
              wa_dip-obsolete = 'ICL_USCNTXT_UPDATE'.
            ENDIF.
            wa_dip-zreplace =  'CL_USER_DEFAULTS=>SAVE_ALL'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_USER_DEFAULTS'
                  im_v_method      = 'SAVE_ALL'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_brf USING 'CL_USER_DEFAULTS' 'SAVE_ALL' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF ( ( ( wa_code CS 'CALL FUNCTION ''ICL_US_CNTXT_GET''') AND wa_code+0(1) NE '*' ) AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''ICL_US_CNTXT_GET'''.
              wa_dip-obsolete = 'ICL_US_CNTXT_GET'.
            ENDIF.
            wa_dip-zreplace =  'CL_USER_DEFAULTS=>GET_SINGLE_VALUE'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_USER_DEFAULTS'
                  im_v_method      = 'GET_SINGLE_VALUE'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_brf USING 'CL_USER_DEFAULTS' 'GET_SINGLE_VALUE' it_param it_exce it_seosubcodf l_tabix it_old_par.
          ELSEIF  ( ( ( wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE''')
            OR ( wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE_46''' )
            OR ( wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE_50''' ) )
            AND wa_code+0(1) NE '*' )  .
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE'''.
              wa_dip-obsolete = 'SCP_READ_CODEPAGE'.
            ELSEIF wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE_46'''.
              wa_dip-obsolete = 'SCP_READ_CODEPAGE_46'.
            ELSEIF wa_code CS 'CALL FUNCTION ''SCP_READ_CODEPAGE_50'''.
              wa_dip-obsolete = 'SCP_READ_CODEPAGE_50'.
            ENDIF.

            wa_dip-zreplace =  'CL_SCP_SEGMENT_UTIL=>READ_CODEPAGE'.
            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prm done by nikhila
            wa_table1-sno = wa_dip-count_no.
            wa_table1-objname = s_prog.
            wa_table1-processed = 'X'.
            INSERT /vshaneya/table1 FROM wa_table1.
            CLEAR wa_table1.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CREATE OBJECT l_i_obj_class_att.
            IF l_i_obj_class_att IS BOUND.
              CALL METHOD l_i_obj_class_att->get_class_attr
                EXPORTING
                  im_v_class       = 'CL_SCP_SEGMENT_UTIL'
                  im_v_method      = 'READ_CODEPAGE'
                IMPORTING
                  ex_it_param      = it_param
                  ex_it_exce       = it_exce
                  ex_it_seosubcodf = it_seosubcodf.
            ENDIF.

            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
            CLEAR wa_code.
            PERFORM replace_fnmodule_class_scp USING 'CL_SCP_SEGMENT_UTIL' 'READ_CODEPAGE' it_param it_exce it_seosubcodf l_tabix it_old_par.
*          ELSEIF  ( (  wa_code CS 'CALL FUNCTION ''RH_CHECK_OM_BIND_PARAM''' ) AND wa_code+0(1) NE '*' )  .
*            l_tabix = sy-tabix.
*            lv_tab1 = sy-tabix.
*            wa_dip-count_no = lv_count + 1.
*            IF wa_code CS 'CALL FUNCTION ''RH_CHECK_OM_BIND_PARAM'''.
*              wa_dip-obsolete = 'RH_CHECK_OM_BIND_PARAM'.
*            ENDIF.
*
*            wa_dip-zreplace =  'CL_SWF_BND_BINDING=>CHECK_ALL'.
*            wa_dip-stfm = 'FUNCTION MODULE REPLACED'.
*            wa_dip-zprogram = ls_report-name.
*            APPEND wa_dip TO it_dip.
*
*            CLEAR wa_dip.
*            CONDENSE wa_code.
*            CONCATENATE '*' wa_code INTO wa_code.
*            CONDENSE wa_code.
*            MODIFY it_code FROM wa_code.
*            CREATE OBJECT l_i_obj_class_att.
*            IF l_i_obj_class_att IS BOUND.
*              CALL METHOD l_i_obj_class_att->get_class_attr
*                EXPORTING
*                  im_v_class       = 'CL_SWF_BND_BINDING'
*                  im_v_method      = 'CHECK_ALL'
*                IMPORTING
*                  ex_it_param      = it_param
*                  ex_it_exce       = it_exce
*                  ex_it_seosubcodf = it_seosubcodf.
*            ENDIF.
*
*            PERFORM get_old_fm_par USING lv_tab1 CHANGING it_old_par.
*            CLEAR wa_code.
*            PERFORM replace_fnmodule_class_scp USING 'CL_SWF_BND_BINDING' 'CHECK_ALL' it_param it_exce it_seosubcodf l_tabix it_old_par.
***
*          ELSEIF ( wa_code CS 'CALL FUNCTION ''WS_UPLOAD''') AND wa_code+0(1) NE '*'.
*            l_tabix = sy-tabix.
*            wa_dip-count_no = lv_count + 1.
*            wa_dip-obsolete = 'WS_UPLOAD'.
*            wa_dip-zreplace =  'GUI_UPLOAD'.
*            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
*            wa_dip-zprogram = ls_report-name.
*            APPEND wa_dip TO it_dip.
*
*            CLEAR wa_dip.
*            CONDENSE wa_code.
*            CONCATENATE '*' wa_code INTO wa_code.
*            CONDENSE wa_code.
*            MODIFY it_code FROM wa_code.
*            PERFORM get_fm_parameters USING lv_tab1 'WS_UPLOAD' wa_code CHANGING param param2 param3 param4 param5 param6.
*            CLEAR wa_code.
*            PERFORM replace_fnmodule USING 'GUI_UPLOAD' l_tabix.
*            CONTINUE.
***Changes for FMa to Class replacement-END
          ELSEIF ( wa_code CS 'CALL FUNCTION ''WS_FILENAME_GET''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'WS_FILENAME_GET'.
            wa_dip-zreplace =  'F4_FILENAME'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING lv_tab1 'WS_FILENAME_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'F4_FILENAME' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''NAMETAB_GET''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'NAMETAB_GET'.
            wa_dip-zreplace =  'DDIF_FIELDINFO_GET'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'DDIF_FIELDINFO_GET' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''COPCA_SET_CO_OBJECTS_FOR_AM''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'COPCA_SET_CO_OBJECTS_FOR_AM'.
            wa_dip-zreplace =  'K_ACCOUNTINGS_SET_FOR_ASSETS'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix 'COPCA_SET_CO_OBJECTS_FOR_AM' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'K_ACCOUNTINGS_SET_FOR_ASSETS' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_RELATIONS_GET'''
            OR wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_RELATIONS_GET''' ) AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_RELATIONS_GET'''.
              wa_dip-obsolete = 'ASH_PM_QMEL_RELATIONS_GET'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_RELATIONS_GET'''.
              wa_dip-obsolete = 'ASH_SM_QMEL_RELATIONS_GET'.
            ENDIF.
            wa_dip-zreplace =  'ASH_QMEL_RELATIONS_GET'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            IF wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_RELATIONS_GET'''.
              PERFORM get_fm_parameters USING lv_tab1 'ASH_PM_QMEL_RELATIONS_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
              CLEAR wa_code.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_RELATIONS_GET'''.
              PERFORM get_fm_parameters USING lv_tab1 'ASH_SM_QMEL_RELATIONS_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
              CLEAR wa_code.
            ENDIF.
            PERFORM replace_fnmodule USING 'ASH_QMEL_RELATIONS_GET' l_tabix.
            CONTINUE.
*****************Begin of changes for CRM Function modules
             ELSEIF ( wa_code CS 'CALL FUNCTION ''CRM_ADDR1_EXTRACT_BW'''
            OR wa_code CS 'CALL FUNCTION ''CRM_ADDR2_EXTRACT_BW'''
            OR wa_code CS 'CALL FUNCTION ''CRM_FS_BRI_FROM_PRIDOC_GEN_EC'''
            OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_COMMIT'''
            OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_CREATE'''
            OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_DELETE'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_EXEC_BO_METHOD'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_GET_QUERY_RESULT'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_INIT'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_INITIALIZE'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_LOCK'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_MODIFY'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_READ'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_RESET'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_ROLLBACK'''
           OR wa_code CS 'CALL FUNCTION ''CRM_GENIL_SAVE'''
           OR wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODES_ABOVE_GET'''
           OR wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODES_BELOW_GET'''
           OR wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODE_READ''' ) AND wa_code+0(1) NE '*'.


            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'CALL FUNCTION ''CRM_ADDR1_EXTRACT_BW'''.
              wa_dip-obsolete = 'CRM_ADDR1_EXTRACT_BW'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_ADDR2_EXTRACT_BW'''.
              wa_dip-obsolete = 'CRM_ADDR2_EXTRACT_BW'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_FS_BRI_FROM_PRIDOC_GEN_EC'''.
              wa_dip-obsolete = 'CRM_FS_BRI_FROM_PRIDOC_GEN_EC'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_COMMIT'''.
              wa_dip-obsolete = 'CRM_GENIL_COMMIT'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_CREATE'''.
              wa_dip-obsolete = 'CRM_GENIL_CREATE'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_DELETE'''.
              wa_dip-obsolete = 'CRM_GENIL_DELETE'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_EXEC_BO_METHOD'''.
              wa_dip-obsolete = 'CRM_GENIL_EXEC_BO_METHOD'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_GET_QUERY_RESULT'''.
              wa_dip-obsolete = 'CRM_GENIL_GET_QUERY_RESULT'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_INIT'''.
              wa_dip-obsolete = 'CRM_GENIL_INIT'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_INITIALIZE'''.
              wa_dip-obsolete = 'CRM_GENIL_INITIALIZE'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_LOCK'''.
              wa_dip-obsolete = 'CRM_GENIL_LOCK'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_MODIFY'''.
              wa_dip-obsolete = 'CRM_GENIL_MODIFY'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_READ'''.
              wa_dip-obsolete = 'CRM_GENIL_READ'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_RESET'''.
              wa_dip-obsolete = 'CRM_GENIL_RESET'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_ROLLBACK'''.
              wa_dip-obsolete = 'CRM_GENIL_ROLLBACK'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_GENIL_SAVE'''.
              wa_dip-obsolete = 'CRM_GENIL_SAVE'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODES_ABOVE_GET'''.
              wa_dip-obsolete = 'CRM_IPM_HIER_NODES_ABOVE_GET'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODES_BELOW_GET'''.
              wa_dip-obsolete = 'CRM_IPM_HIER_NODES_BELOW_GET'.
            ENDIF.
            IF wa_code CS 'CALL FUNCTION ''CRM_IPM_HIER_NODE_READ'''.
              wa_dip-obsolete = 'CRM_IPM_HIER_NODE_READ'.
            ENDIF.

            wa_dip-zreplace =  'Not Applicable for Replacement'.
            wa_dip-stfm = 'FUNCTION MOUDLE NOT APPLICABLE'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
*            CONDENSE wa_code.
*            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
*            IF wa_code CS 'CALL FUNCTION ''ASH_PM_QMEL_RELATIONS_GET'''.
*              PERFORM get_fm_parameters USING lv_tab1 'ASH_PM_QMEL_RELATIONS_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
*              CLEAR wa_code.
*            ENDIF.
*            IF wa_code CS 'CALL FUNCTION ''ASH_SM_QMEL_RELATIONS_GET'''.
*              PERFORM get_fm_parameters USING lv_tab1 'ASH_SM_QMEL_RELATIONS_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
*              CLEAR wa_code.
*            ENDIF.
*            PERFORM replace_fnmodule USING 'ASH_QMEL_RELATIONS_GET' l_tabix.
            CONTINUE.
********************************************************END********************************************************************************
          ELSEIF ( wa_code CS 'CALL FUNCTION ''FS_MAP_BAPI_CUREXT_CONV_TO_EXT''') AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            lv_tab1 = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'FS_MAP_BAPI_CUREXT_CONV_TO_EXT'.
            wa_dip-zreplace =  'BAPI_CURRENCY_CONV_TO_EXTERN_9'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING lv_tab1 'FS_MAP_BAPI_CUREXT_CONV_TO_EXT' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_CURRENCY_CONV_TO_EXTERN_9' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'CALL FUNCTION ''HELP_VALUES_GET''' OR wa_code CS 'CALL FUNCTION ''HELP_VALUES_GET_EXTEND''' ) AND wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'HELP_VALUES_GET_EXTEND'.
              wa_dip-obsolete = 'HELP_VALUES_GET_EXTEND'.
            ENDIF.
            IF wa_code CS 'HELP_VALUES_GET'.
              wa_dip-obsolete = 'HELP_VALUES_GET'.
            ENDIF.
            wa_dip-zreplace =  'F4IF_FIELD_VALUE_REQUEST'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'HELP_VALUES_GET' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'F4IF_FIELD_VALUE_REQUEST' l_tabix.
            CONTINUE.

*          ELSEIF ( wa_code CS 'CALL FUNCTION ''HELP_VALUES_GET_NO_DD_NAME'''
*                   OR wa_code CS 'CALL FUNCTION ''HELP_VALUES_GET_WITH_DD_NAME'''
*            OR wa_code CS '''HELP_VALUES_GET_WITH_TABLE_EXT''' OR  wa_code CS '''HELP_VALUES_GET_WITH_TABLE''') AND wa_code+0(1) NE '*'.
*            l_tabix = sy-tabix.
*            wa_dip-count_no = lv_count + 1.
*            IF wa_code CS 'HELP_VALUES_GET_WITH_DD_NAME'.
*              wa_dip-obsolete = 'HELP_VALUES_GET_WITH_DD_NAME'.
*            ENDIF.
*            IF wa_code CS 'HELP_VALUES_GET_NO_DD_NAME'.
*              wa_dip-obsolete = 'HELP_VALUES_GET_WITH_DD_NAME'.
*            ENDIF.
*            IF wa_code CS 'HELP_VALUES_GET_WITH_TABLE_EXT'.
*              wa_dip-obsolete = 'HELP_VALUES_GET_WITH_TABLE_EXT'.
*            ENDIF.
*            wa_dip-zreplace =  'F4IF_INT_TABLE_VALUE_REQUEST'.
*            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
*            wa_dip-zprogram = ls_report-name.
*            APPEND wa_dip TO it_dip.
*
*            CLEAR wa_dip.
*            CONDENSE wa_code.
*            CONCATENATE '*' wa_code INTO wa_code.
*            CONDENSE wa_code.
*            MODIFY it_code FROM wa_code.
*            CLEAR wa_code.
*            PERFORM replace_fnmodule USING 'F4IF_INT_TABLE_VALUE_REQUEST' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_ACCOUNT_CREATE''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_ACCOUNT_CREATE'.
            wa_dip-zreplace =  'BAPI_CD_ACCOUNT_CREATE1'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BAPI_CD_ACCOUNT_CREATE' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_CD_ACCOUNT_CREATE1' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_ACCOUNT_GETDETAIL''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_ACCOUNT_GETDETAIL'.
            wa_dip-zreplace =  'BAPI_CD_ACCOUNT_GETDETAIL1'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BAPI_CD_ACCOUNT_GETDETAIL' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_CD_ACCOUNT_GETDETAIL1'  l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_ACCOUNT_CHANGE''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_ACCOUNT_CHANGE'.
            wa_dip-zreplace =  'BAPI_CD_ACCOUNT_CHANGE1'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BAPI_CD_ACCOUNT_CHANGE' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_CD_ACCOUNT_CHANGE1' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_INSOBJ_CHANGE''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_INSOBJ_CHANGE'.
            wa_dip-zreplace =  'BAPI_INSOBJECT_CHANGE'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BAPI_CD_INSOBJ_CHANGE' wa_code CHANGING param param2 param3 param4 param5 param6.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_INSOBJECT_CHANGE' l_tabix..
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_INSOBJ_CREATE''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_INSOBJ_CREATE'.
            wa_dip-zreplace =  'BAPI_INSOBJECT_CREATE'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_INSOBJECT_CREATE' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BAPI_CD_INSOBJ_GETDETAIL''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_CD_INSOBJ_GETDETAIL'.
            wa_dip-zreplace =  'BAPI_INSOBJECT_GETDETAIL'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BAPI_INSOBJECT_GETDETAIL' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'FMCA_DUNNING_PF_NO_DET_0350''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'FMCA_DUNNING_PF_NO_DET_0350'.
            wa_dip-zreplace =  'FKK_DUNNING_PF_NO_DET_0350'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.

            MODIFY it_code FROM wa_code.

            PERFORM get_fm_parameters USING l_tabix  'FMCA_DUNNING_PF_NO_DET_0350' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'FKK_DUNNING_PF_NO_DET_0350' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'FMCA_SAMPLE_ROUND_2040''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'FMCA_SAMPLE_ROUND_2040'.
            wa_dip-zreplace =  'FKK_SAMPLE_ROUND_2040'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'FKK_SAMPLE_ROUND_2040' l_tabix.
            CONTINUE.

          ELSEIF ( wa_code CS 'BAPI_BUPA_FS_CREATE_FROM_DATA''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BAPI_BUPA_FS_CREATE_FROM_DATA'.
            wa_dip-zreplace =  'BUPA_CREATE_FROM_DATA'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BUPA_CREATE_FROM_DATA' l_tabix.
            CONTINUE.

          ELSEIF ( wa_code CS 'MIGO_GR_CREATE''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'MIGO_GR_CREATE'.
            wa_dip-zreplace =  'MIGO_DIALOG'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'MIGO_GR_CREATE' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'MIGO_DIALOG' l_tabix..
            CONTINUE.
          ELSEIF ( wa_code CS 'UPDATE_PROFITCENTER_BUKRS''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'UPDATE_PROFITCENTER_BUKRS'.
            wa_dip-zreplace =  'K_PROFITCENTER_POST'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'K_PROFITCENTER_POST' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BUPA_ROLES_GET''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BUPA_ROLES_GET'.
            wa_dip-zreplace =  'BUPA_ROLES_GET_2'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BUPA_ROLES_GET' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BUPA_ROLES_GET_2' l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'BUPA_ROLE_ADD''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BUPA_ROLE_ADD'.
            wa_dip-zreplace =  'BUPA_ROLE_ADD_2'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BUPA_ROLE_ADD' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BUPA_ROLE_ADD_2'  l_tabix.
*            CONTINUE.

          ELSEIF ( wa_code CS 'BUPA_ROLE_EXISTENCE_CHECK''') AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'BUPA_ROLE_EXISTENCE_CHECK'.
            wa_dip-zreplace =  'BUPA_ROLE_EXISTENCE_CHECK_2'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*            INSERT /vshaneya/zprg FROM wa_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            PERFORM get_fm_parameters USING l_tabix  'BUPA_ROLE_EXISTENCE_CHECK' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'BUPA_ROLE_EXISTENCE_CHECK_2' l_tabix.
            CONTINUE.

          ELSEIF ( wa_code CS 'POPUP_TO_CONFIRM_WITH_MESSAGE' OR wa_code CS 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
                OR wa_code CS 'POPUP_TO_CONFIRM_STEP' OR wa_code CS 'POPUP_TO_CONFIRM_WITH_VALUE'
                 OR wa_code CS 'POPUP_TO_DECIDE' OR wa_code CS 'POPUP_TO_DECIDE_WITH_MESSAGE' ) AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            IF wa_code CS 'POPUP_TO_CONFIRM_WITH_MESSAGE'.
              wa_dip-obsolete = 'POPUP_TO_CONFIRM_WITH_MESSAGE'.
            ENDIF.
            IF wa_code CS 'POPUP_TO_CONFIRM_LOSS_OF_DATA'.
              wa_dip-obsolete = 'POPUP_TO_CONFIRM_LOSS_OF_DATA'.
            ENDIF.
            IF wa_code CS 'POPUP_TO_CONFIRM_STEP'.
              wa_dip-obsolete = 'POPUP_TO_CONFIRM_STEP'.
            ENDIF.
            IF  wa_code CS 'POPUP_TO_CONFIRM_WITH_VALUE'.
              wa_dip-obsolete = 'POPUP_TO_CONFIRM_WITH_VALUE'.
            ENDIF.
            IF wa_code CS 'POPUP_TO_DECIDE'.
              wa_dip-obsolete = 'POPUP_TO_DECIDE'.
            ENDIF.
            IF wa_code CS 'POPUP_TO_DECIDE_WITH_MESSAGE'.
              wa_dip-obsolete = 'POPUP_TO_DECIDE_WITH_MESSAGE'.
            ENDIF.
            wa_dip-zreplace =  'POPUP_TO_CONFIRM'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code '"Commented by Haneya Tool' INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            IF  wa_code CS 'POPUP_TO_CONFIRM_WITH_VALUE'.
              PERFORM get_fm_parameters USING l_tabix  'POPUP_TO_CONFIRM_WITH_VALUE' wa_code CHANGING param param2 param3 param4 param5 param6 .
              CLEAR wa_code.
            ELSE.
              PERFORM get_fm_parameters USING l_tabix  'POPUP_TO_DECIDE' wa_code CHANGING param param2 param3 param4 param5 param6 .
              CLEAR wa_code.
            ENDIF.
            PERFORM replace_fnmodule USING 'POPUP_TO_CONFIRM'   l_tabix.
            CONTINUE.
          ELSEIF ( wa_code CS 'HR_MRU_SET_MOLGA' ) AND  wa_code+0(1) NE '*'.

            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'HR_MRU_SET_MOLGA'.
            wa_dip-zreplace =  'HR_M99_SET_MOLGA'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code.
            CLEAR wa_code.
            wa_code = 'CALL FUNCTION ''HR_M99_SET_MOLGA'' .'.
            INSERT wa_code INTO it_code.
            CLEAR wa_code.
            CONTINUE.
          ELSEIF ( wa_code CS 'POPUP_TO_GET_VALUE' ) AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'POPUP_TO_GET_VALUE'.
            wa_dip-zreplace =  'POPUP_GET_VALUES'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes
            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code .
            PERFORM get_fm_parameters USING l_tabix  'POPUP_TO_GET_VALUE' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'POPUP_GET_VALUES' l_tabix.
*            PERFORM popup_get_newfm USING 'POPUP_TO_GET_VALUE' 'POPUP_GET_VALUES'  .
            CONTINUE.
          ELSEIF ( wa_code CS 'RK_PROFITCENTER_UPDATE' ) AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'RK_PROFITCENTER_UPDATE'.
            wa_dip-zreplace =  'K_PROFITCENTER_POST'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CLEAR wa_dip.
            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'K_PROFITCENTER_POST' l_tabix.
            CONTINUE.

          ELSEIF ( wa_code CS 'ISCD_INSOBJECT_MAINTAIN' ) AND  wa_code+0(1) NE '*'.
            l_tabix = sy-tabix.
            wa_dip-count_no = lv_count + 1.
            wa_dip-obsolete = 'ISCD_INSOBJECT_MAINTAIN'.
            wa_dip-zreplace =  'FSCD_INSOBJECT_MAINTAIN'.
            wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
            wa_dip-zprogram = ls_report-name.
            wa_dip-line = sy-tabix.
            APPEND wa_dip TO it_dip.
*            INSERT /vshaneya/zprg FROM wa_dip.
            CLEAR wa_dip.
*changes for dashboard prgm done by nikhila
            wa_table2-sno = wa_dip-count_no.
            wa_table2-objname = s_prog.
            wa_table2-processed = 'X'.
            INSERT /vshaneya/table2 FROM wa_table2.
            CLEAR wa_table2.
*end of changes

            CONDENSE wa_code.
            CONCATENATE '*' wa_code INTO wa_code.
            CONDENSE wa_code.
            MODIFY it_code FROM wa_code .
            PERFORM get_fm_parameters USING l_tabix  'ISCD_INSOBJECT_MAINTAIN' wa_code CHANGING param param2 param3 param4 param5 param6 .
            CLEAR wa_code.
            PERFORM replace_fnmodule USING 'FSCD_INSOBJECT_MAINTAIN' l_tabix.

            CONTINUE.
          ENDIF.
        ENDIF.

***************************END OF REPLACE OF OBSOLETE STATEMENTS****************************************************

* Change by Vinod Vsoft.

      ENDLOOP.
**********************************************

    ENDIF.
endloop.

    IF  it_code1 IS INITIAL.

      LOOP AT it_code INTO wa_code.
        wa_codet-code = wa_code.
        wa_codet-line = sy-tabix.
        APPEND wa_codet TO it_codet.
        CLEAR wa_codet.
      ENDLOOP.
      LOOP AT it_code INTO wa_code.

        IF  wa_code CS '*CALL FUNCTION' AND wa_code+1(1) NS '*' .
          DATA lv_tab TYPE sy-tabix.
          lv_tab = sy-tabix.
          DATA lv_inc  TYPE i.
          DATA lv_in TYPE i.
          lv_tab = lv_tab + 1.
          LOOP AT it_codet INTO wa_codet FROM lv_tab.
            CONCATENATE '*' wa_codet-code INTO wa_code .
            MODIFY it_code INDEX wa_codet-line FROM wa_code.
            IF wa_codet-code CS '.'.
              lv_in = lv_in + 1.
              lv_inc = lv_inc + lv_in.
              wa_code = '*End of change by Haneya Tool'.
              INSERT wa_code INTO it_code  INDEX wa_codet-line + lv_inc.
              wa_codet-code = wa_code.
              INSERT wa_codet INTO it_codet  INDEX wa_codet-line + lv_inc.
              EXIT.

            ENDIF.
          ENDLOOP.
          CLEAR :lv_tab,lv_inc.

        ENDIF.
      ENDLOOP.

******************************************************************************
*      PERFORM pretty_printer CHANGING it_code.
*      IF p_rem = abap_true.
*        INSERT REPORT ls_report-name FROM it_code.
*        IF sy-subrc = 0.
*          APPEND wa_dip TO it_dip.
*          SORT it_dip BY count_no obsolete.
*          DELETE ADJACENT DUPLICATES FROM it_dip.
*          DELETE it_dip WHERE count_no EQ 0.
*          MODIFY /vshaneya/zprg FROM TABLE it_dip.
*
**        INSERT /vshaneya/zprg FROM wa_dip.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*      IF p_rem = abap_true.
*        INSERT REPORT ls_report-name  FROM it_code1.
*
*        IF sy-subrc = 0.
*          APPEND wa_dip TO it_dip.
*          SORT it_dip BY count_no obsolete.
***************************************************************************
**          INSERT /vshaneya/zprg FROM TABLE it_dip.
*          MODIFY /vshaneya/zprg FROM TABLE it_dip.
**************************************************************************
**        INSERT /vshaneya/zprg FROM wa_dip.
*        ENDIF.
*      ENDIF.
*      CLEAR  it_code1[].
*    ENDIF.
**    ENDIF.
*
*    CLEAR : it_code1[],it_code[],wa_code,it_codet[].
*    CLEAR ls_report.
*  ENDLOOP.
********************************************************************************

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
                    "vinod change's
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


      PERFORM pretty_printer CHANGING it_code.
      IF p_rem = abap_true.
        INSERT REPORT ls_report-name FROM it_code.
        IF sy-subrc = 0.
          APPEND wa_dip TO it_dip.
          CLEAR wa_dip.
          SORT it_dip BY count_no obsolete.
          DELETE ADJACENT DUPLICATES FROM it_dip.
          DELETE it_dip WHERE count_no EQ 0.
          MODIFY /vshaneya/zprg FROM TABLE it_dip.
        ENDIF.
      ENDIF.

    ELSE.

      PERFORM pretty_printer CHANGING it_code1.
      IF p_rem = abap_true.
        INSERT REPORT ls_report-name  FROM it_code1.
        IF sy-subrc = 0.
          APPEND wa_dip TO it_dip.
          CLEAR wa_dip.
          SORT it_dip BY count_no obsolete.
          DELETE ADJACENT DUPLICATES FROM it_dip.
          DELETE it_dip WHERE count_no EQ 0.
          MODIFY /vshaneya/zprg FROM TABLE it_dip.
*         NSERT /vshaneya/zprg FROM wa_dip.
*************************************************************************

        ENDIF.
      ENDIF.
      CLEAR  it_code1[].
    ENDIF.
*   ENDIF.
    CLEAR : it_code1[],it_code[],wa_code,it_codet[].
    CLEAR ls_report.
*  ENDLOOP.



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF it_dip[] IS NOT INITIAL.
    PERFORM display.
  ELSE.
    MESSAGE 'No Obsolete Syntaxes for Remediation' TYPE 'I'.
*    PERFORM display1.
  ENDIF.


**&---------------------------------------------------------------------*
**&      Form  REPLACE_CODE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM REPLACE_CODE .
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-042.
*  CONDENSE WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-040.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  CLEAR: L_VAR1 , L_VAR2.
*
*  LOOP AT IT_CODE INTO WA_CODE1.
*    IF SY-TABIX GT L_TABIX.
*      IF WA_CODE1 CS 'FILENAME'.
*        SPLIT WA_CODE1 AT '=' INTO L_VAR1 L_VAR2.
*        CONCATENATE TEXT-041 '=' L_VAR2 '.' INTO WA_CODE1 SEPARATED BY SPACE.
*        APPEND WA_CODE1 TO IT_CODE1.
*        CLEAR WA_CODE1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*
*  WA_CODE1 = TEXT-001.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-002.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-003.
*  SHIFT WA_CODE1 LEFT DELETING LEADING '*'.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-004.
*  SHIFT WA_CODE1 LEFT DELETING LEADING '*'.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-005.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-006.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-007.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-008.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-009.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-010.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-011.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-012.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-013.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-014.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-015.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-016.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-017.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-018.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  LOOP AT IT_CODE INTO WA_CODE1.
*    IF SY-TABIX GT L_TABIX.
*      IF WA_CODE1 CS 'DATA_TAB'.
*        SHIFT WA_CODE1 LEFT DELETING LEADING '*'.
**  wa_code1 = wa_code ."text-019.
*        APPEND WA_CODE1 TO IT_CODE1.
*        CLEAR WA_CODE1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  WA_CODE1 = TEXT-020.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-021.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-022.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-023.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-024.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-025.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-026.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-027.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-028.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-029.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-030.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-031.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-032.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-033.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-034.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-035.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-036.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-037.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-038.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-039.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REPLACE_POPUP_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM REPLACE_POPUP_MSG . "from text element -044
*
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-042.
*  CONDENSE WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-070.
*  CONDENSE WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-071.
*  CONDENSE WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-072.
*  CONDENSE WA_CODE1.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  CLEAR : L_VAR1, L_VAR2.
*  LOOP AT IT_CODE INTO WA_CODE1.
*    IF WA_CODE1 CS 'TITEL'.
*      SPLIT WA_CODE1 AT '=' INTO L_VAR1 L_VAR2.
*      CONCATENATE  TEXT-068 '=' L_VAR2 '.' INTO WA_CODE1 SEPARATED BY SPACE.
*      APPEND WA_CODE1 TO IT_CODE1.
*      CLEAR WA_CODE1.
*    ENDIF.
*  ENDLOOP.
*
*  DATA : L_DAI1  TYPE STRING,
*         L_DAI2  TYPE STRING,
*         L_DAI3  TYPE STRING,
*         L_TEXT1 TYPE STRING,
*         L_TEX1  TYPE STRING,
*         L_TEXT  TYPE STRING,
*         L_TEXT2 TYPE STRING.
*
*  CLEAR : L_DAI1, L_DAI2,L_DAI3,L_TEXT1,L_TEXT2.
*
*  CLEAR : L_VAR1, L_VAR2.
*  LOOP AT IT_CODE INTO WA_CODE1.
*    IF WA_CODE1 CS 'DIAGNOSETEXT1'.
**      SPLIT wa_code1 AT '=' INTO l_var1 l_dai1.
**      LOOP AT it_code INTO wa_code.
**        IF wa_code CS l_dai1.
**          CLEAR :l_var1, l_dai1.
**          SPLIT wa_code AT 'VALUE' INTO l_var1 l_dai1.
**          SPLIT l_text1 AT ',' INTO l_dai1 l_var1.
**          IF sy-subrc NE 0.
**            SPLIT l_text1 AT '.' INTO l_dai1 l_var1.
**          ENDIF.
**        ENDIF.
**      ENDLOOP.
*    ELSEIF WA_CODE1 CS 'DIAGNOSETEXT2'.
**      SPLIT wa_code1 AT '=' INTO l_var1 l_dai2.
**      LOOP AT it_code INTO wa_code.
**        IF wa_code CS l_dai2.
**          CLEAR :l_var1, l_dai2.
**          SPLIT wa_code AT 'VALUE' INTO l_var1 l_dai2.
**          SPLIT l_text1 AT ',' INTO l_dai2 l_var1.
**          IF sy-subrc NE 0.
**            SPLIT l_text1 AT '.' INTO l_dai2 l_var1.
**          ENDIF.
**        ENDIF.
**      ENDLOOP.
*    ELSEIF WA_CODE1 CS 'DIAGNOSETEXT3'.
**      SPLIT wa_code1 AT '=' INTO l_var1 l_dai3.
*    ELSEIF WA_CODE1 CS 'TEXTLINE1'.
*      SPLIT WA_CODE1 AT '=' INTO L_VAR1 L_TEXT1.
*    ELSEIF WA_CODE1 CS 'TEXTLINE2'.
*      SPLIT WA_CODE1 AT '=' INTO L_VAR1 L_TEXT2.
*    ENDIF.
*  ENDLOOP.
*
*  DATA : L_TEXTX TYPE STRING VALUE 'l_text'.
*
**  CONCATENATE l_dai1 l_dai2 l_dai3 l_text1 l_text2  INTO l_tex1 SEPARATED BY space.
**  CONCATENATE l_text1 l_text2  INTO l_tex1 SEPARATED BY space.
*  CONDENSE L_TEXTX.
*  CONCATENATE L_TEXTX '=' L_TEX1 INTO WA_CODE1 SEPARATED BY SPACE.
*
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-044.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-045.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-046.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-047.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-048.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-049.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-050.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-051.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-052.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-053.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-054.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-055.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-056.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-057.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-058.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-059.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-060.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-061.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*  WA_CODE1 = TEXT-062.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-063.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-064.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-065.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-066.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*  WA_CODE1 = TEXT-067.
*  APPEND WA_CODE1 TO IT_CODE1.
*  CLEAR WA_CODE1.
*
*
*  CLEAR: L_VAR1 , L_VAR2.
*
*  LOOP AT IT_CODE INTO WA_CODE1.
*    IF SY-TABIX GT L_TABIX.
*      IF WA_CODE1 CS 'ANSWER'.
*        SPLIT WA_CODE1 AT '=' INTO L_VAR1 L_VAR2.
*        IF L_VAR2 CS '.'.
*          SPLIT L_VAR2 AT '.' INTO L_VAR2 L_VAR1.
*        ENDIF.
*        CONCATENATE L_VAR2 '=' TEXT-069 INTO WA_CODE1 SEPARATED BY SPACE.
*        APPEND WA_CODE1 TO IT_CODE1.
*        CLEAR WA_CODE1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REPLACE_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM replace_filename .
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-042.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-073.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-074.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-075.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-076.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-077.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-078.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  wa_code1 = TEXT-079.
  APPEND wa_code1 TO it_code1.
  CLEAR:wa_code1.

  LOOP AT it_code INTO wa_code1.
    DATA: str1 TYPE string,
          str2 TYPE string,
          str3 TYPE string.
    str3 = 'FILE_NAME'.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'FILENAME =' AND wa_code1 NS '*DEF_FILENAME = '.
        SHIFT wa_code1 LEFT DELETING LEADING '*'.
        SPLIT wa_code1 AT '=' INTO str1 str2.
        CONCATENATE str3 '  =' str2 INTO wa_code1.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.

        wa_code1 = TEXT-080 .
        APPEND wa_code1 TO it_code1.
        CLEAR: wa_code1.

      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REPLACE_NAMESTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM replace_namestab .

  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-042.
  CONDENSE wa_code1.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-081.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-082.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.


  CLEAR: l_var1 , l_var2.
  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'tabname'.
        SPLIT wa_code1 AT '=' INTO l_var1 l_var2.
        CONCATENATE TEXT-083  l_var2 INTO wa_code1 SEPARATED BY space.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

  wa_code1 = TEXT-084.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-085.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-086.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-087.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-088.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-089.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-090.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-091.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  CLEAR: l_var1 , l_var2.
  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'HEADER'.
        SPLIT wa_code1 AT '=' INTO l_var1 l_var2.
        CONCATENATE TEXT-092  l_var2 INTO wa_code1 SEPARATED BY space.
        SHIFT wa_code1 LEFT DELETING LEADING '*'.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.


  wa_code1 = TEXT-093.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-094.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-095.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-096.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.


  CLEAR: l_var1 , l_var2.
  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'nametab'.
        SPLIT wa_code1 AT '=' INTO l_var1 l_var2.
        CONCATENATE TEXT-097  l_var2 INTO wa_code1 SEPARATED BY space.
        SHIFT wa_code1 LEFT DELETING LEADING '*'.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
      ENDIF.
    ENDIF.

  ENDLOOP.


  wa_code1 = TEXT-098.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-099.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-100.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-101.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-102.
  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-103.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .
  DATA: it_fcat TYPE slis_t_fieldcat_alv,
        wa_fcat TYPE slis_fieldcat_alv,
        l_repid TYPE syrepid.

* Initialzation
  CLEAR:  wa_fcat.

  REFRESH: it_fcat.

  l_repid = sy-repid.



  wa_fcat-col_pos = 1.
  wa_fcat-fieldname = 'ZPROGRAM'.
  wa_fcat-tabname = 'IT_DIP'.
  wa_fcat-seltext_m = 'Program Name'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 2.
  wa_fcat-fieldname = 'STFM'.
  wa_fcat-tabname = 'IT_DIP'.
  wa_fcat-seltext_l = 'Statement/Function Module'(165).
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 3.
  wa_fcat-fieldname = 'OBSOLETE'.
  wa_fcat-tabname = 'IT_DIP'.
  wa_fcat-seltext_m = 'Obsolete'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 4.
  wa_fcat-fieldname = 'ZREPLACE'.
  wa_fcat-tabname = 'IT_DIP'.
  wa_fcat-seltext_m = 'Replace'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.


  DATA: wa_layout TYPE slis_layout_alv.
  CLEAR wa_layout.
  wa_layout-zebra = 'X'.
  wa_layout-colwidth_optimize = 'X'.
  SORT it_dip BY zprogram.
*  delete it_dip WHERE obsolete = 'ADD SUBTRACT'.
*  DELETE ADJACENT DUPLICATES FROM it_dip COMPARING obsolete.
  IF it_dip[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        is_layout                = wa_layout
        i_callback_pf_status_set = 'SUB_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        it_fieldcat              = it_fcat
      TABLES
        t_outtab                 = it_dip
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ELSEIF it_dip[] IS INITIAL.
    MESSAGE 'No Obsolete statements for Remediation' TYPE 'I'.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display1 .
  DATA: it_fcat TYPE slis_t_fieldcat_alv,
        wa_fcat TYPE slis_fieldcat_alv,
        l_repid TYPE syrepid.

* Initialzation
  DATA: it_dip1 TYPE TABLE OF ty_dip,
        wa_dip1 TYPE ty_dip.
  SELECT * FROM /vshaneya/zprg INTO TABLE it_dip1.

*  loop at it_dip1 into wa_dip1 where COUNT_NO eq space.
*    DELETE it_dip1 from wa_dip1.
*  endloop.

  CLEAR:  wa_fcat.

  REFRESH: it_fcat.

  l_repid = sy-repid.
  CLEAR wa_fcat.
  wa_fcat-col_pos = 1.
  wa_fcat-fieldname = 'ZPROGRAM'.
  wa_fcat-tabname = 'IT_DIP1'.
  wa_fcat-seltext_m = 'Program Name'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 2.
  wa_fcat-fieldname = 'STFM'.
  wa_fcat-tabname = 'IT_DIP1'.
  wa_fcat-seltext_m = 'Statement/Function Module'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 3.
  wa_fcat-fieldname = 'OBSOLETE'.
  wa_fcat-tabname = 'IT_DIP1'.
  wa_fcat-seltext_m = 'Obsolete'.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos = 4.
  wa_fcat-fieldname = 'ZREPLACE'.
  wa_fcat-tabname = 'IT_DIP1'.
  wa_fcat-seltext_m = 'Replace'.
  APPEND wa_fcat TO it_fcat.


  DATA: wa_layout TYPE slis_layout_alv.
  CLEAR wa_layout.
  wa_layout-zebra = 'X'.
  wa_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      is_layout                = wa_layout
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      it_fieldcat              = it_fcat
    TABLES
      t_outtab                 = it_dip1
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_code .
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-042.
  CONDENSE wa_code1.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-040.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  CLEAR: l_var1 , l_var2.

  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'FILENAME'.
        SPLIT wa_code1 AT '=' INTO l_var1 l_var2.
        CONCATENATE TEXT-041 '=' l_var2 '.' INTO wa_code1 SEPARATED BY space.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
      ENDIF.
    ENDIF.
  ENDLOOP.


  wa_code1 = TEXT-104.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-105.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-106.
*  SHIFT wa_code1 LEFT DELETING LEADING '*'.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  CLEAR: l_var1 , l_var2.

  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'FILENAME'.
        SPLIT wa_code1 AT '=' INTO l_var1 l_var2.
        CONCATENATE TEXT-107 l_var2 INTO wa_code1 SEPARATED BY space.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  wa_code1 = text-107.
*  SHIFT wa_code1 LEFT DELETING LEADING '*'.
*  APPEND wa_code1 TO it_code1.
*  CLEAR wa_code1.

  wa_code1 = TEXT-108.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  wa_code1 = TEXT-109.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-110.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-111.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-112.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-113.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-114.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-115.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-116.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-117.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-118.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-119.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-120.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-121.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
*
*  LOOP AT it_code INTO wa_code1.
*    IF sy-tabix GT l_tabix.
*      IF wa_code1 CS 'DATA_TAB'.
*        SHIFT wa_code1 LEFT DELETING LEADING '*'.
**  wa_code1 = wa_code ."text-019.
*        APPEND wa_code1 TO it_code1.
*        CLEAR wa_code1.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  wa_code1 = TEXT-122.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-123.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-124.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-125.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-126.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-127.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-128.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-129.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-130.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-131.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-132.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-133.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  LOOP AT it_code INTO wa_code1.
    IF sy-tabix GT l_tabix.
      IF wa_code1 CS 'DATA_TAB'.
        SHIFT wa_code1 LEFT DELETING LEADING '*'.
*  wa_code1 = wa_code ."text-019.
        APPEND wa_code1 TO it_code1.
        CLEAR wa_code1.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  wa_code1 = text-032.
*  APPEND wa_code1 TO it_code1.
*  CLEAR wa_code1.
  wa_code1 = TEXT-135.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-136.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-137.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-138.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-139.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-140.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-141.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-142.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-143.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-144.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-145.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-146.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-147.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-148.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-149.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-150.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-151.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-152.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-153.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-154.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-155.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-156.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-157.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-158.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.
  wa_code1 = TEXT-159.
  APPEND wa_code1 TO it_code1.
  CLEAR wa_code1.

  APPEND wa_code1 TO it_code1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REPLACE_FNMODULE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM replace_fnmodule USING p_name TYPE thead-tdname p_tabix TYPE sy-tabix.

  DATA gt_line TYPE TABLE OF tline.
  DATA gs_line TYPE  tline.
  DATA lv_tabix TYPE sy-tabix.

  DATA : lv_id     LIKE  thead-tdid,
         lv_lang   TYPE thead-tdspras,
         lv_name   TYPE thead-tdname,
         lv_object TYPE thead-tdobject.
  DATA : it_code3 TYPE TABLE OF string,
         wa_code3 TYPE string.

  DATA fname TYPE rs38l-name.
  DATA lt_source TYPE rswsourcet.
  DATA ls_source TYPE string.
  fname = p_name.

  CALL FUNCTION 'FUNCTION_STUB_GENERATE'
    EXPORTING
      funcname           = fname
*     IC_MODE            = 'X'
*     WITH_ENHANCEMENTS  = 'X'
    TABLES
      source             = lt_source
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.

*********************************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX.      " Vinod
******************************************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.

******************************************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .              "  Vinod
******************************************************************************
  IF p_name EQ 'POPUP_GET_VALUES'.
    wa_code1 = 'DATA : GT_FIELD TYPE STANDARD TABLE OF SVAL.'.
**************************************************************
*    INSERT wa_code1 INTO it_code.
    INSERT wa_code1 INTO TABLE it_code.              " vinod
****************************************************************************
    wa_code1 = 'DATA : GS_FIELD TYPE SVAl.'.
*************************************************************************
*    INSERT wa_code1 INTO it_code.
    INSERT wa_code1 INTO TABLE it_code.           " vinod
***********************************************************************

    CONCATENATE 'GS_FIELD-FIELDNAME =' param '.' INTO wa_code1.
*    wa_code1 = 'GS_FIELD-FIELDNAME = ''''.'.

************************************************************************
*    INSERT wa_code1 INTO it_code.
    INSERT wa_code1 INTO TABLE it_code.       " vinod
***********************************************************************
    CONCATENATE 'GS_FIELD-TABNAME =' param2 '.' INTO wa_code1.
*    wa_code1 = 'GS_FIELD-TABNAME = ''''.'.

**********************************************************************
*    INSERT wa_code1 INTO it_code.
    INSERT wa_code1 INTO TABLE it_code.       " vinod
*****************************************************************

  ENDIF.

  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp EQ space.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
*    IF p_name EQ 'GUI_UPLOAD'.
    IF ls_source CS 'FILENAME' AND ls_source NS 'DEF_'  AND ls_source NS 'CALL'.
      CONCATENATE 'FILENAME = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'FILE_NAME' AND ls_source NS 'DEF_'  AND ls_source NS 'CALL'.
      CONCATENATE 'FILE_NAME = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'DEF_FILENAME' AND ls_source NS 'CALL' .
      CONCATENATE 'DEF_FILENAME = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'FILETYPE'.
      CONCATENATE 'FILETYPE = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'HEADER_LENGTH'.
      CONCATENATE 'HEADER_LENGTH = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'DATA_TAB'.
      CONCATENATE 'DATA_TAB = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
*    ENDIF.
    IF p_name EQ 'K_ACCOUNTINGS_SET_FOR_ASSETS'.

      IF ls_source CS 'T_ACCIT' AND ls_source NS 'AMCO'.
        CONCATENATE 'T_ACCIT = ' param INTO wa_code1 SEPARATED BY space.

      ENDIF.
      IF ls_source CS 'T_ACCIT_AMCO' AND gv_amco IS NOT INITIAL.
        CONCATENATE 'T_ACCIT_AMCO = ' param2 INTO wa_code1 SEPARATED BY space.

      ENDIF.
    ENDIF.
    IF ls_source CS 'FIELDS' AND ls_source NS 'ERROR' AND ls_source NS 'CONTROL'.
      CONCATENATE 'FIELDS = '  'GT_FIELD' INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF ls_source CS 'INSOBJECT' AND ls_source NS 'CALL'.

      IF param6 IS NOT INITIAL AND gv_c IS NOT INITIAL.
        CONCATENATE 'INSOBJECT = ' param6 INTO wa_code1 SEPARATED BY space.
      ELSEIF gv_acct EQ 'X'.
        CONCATENATE 'INSOBJECT = ' param6 INTO wa_code1 SEPARATED BY space.
      ELSEIF gv_objc EQ c_x  AND ls_source NS 'DATA' AND ls_source NS 'DATAX' AND gs_code NS 'OORRX' AND ls_source NS 'CORR' AND ls_source NS 'INSOBJECTPARTNERX'
                           AND ls_source NS 'LOCKX' AND ls_source NS 'LOCK'   AND ls_source NS 'ASSGNX' AND ls_source NS 'ASSGN'
                            AND ls_source NS 'BROKERX' AND ls_source NS 'BROKER' AND ls_source NS 'PAYX' AND ls_source NS 'PAY' AND ls_source NS 'INSOBJNUMBER'
          AND ls_source NS 'WHV' AND ls_source NS 'WHVX'.
        CONCATENATE 'INSOBJECT = ' param INTO wa_code1 SEPARATED BY space.
      ELSEIF gv_ins_gt EQ c_x.
        CONCATENATE 'INSOBJECT = ' param2 INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'BUSPARTNER' AND ls_source NS 'EXT'.
      CONCATENATE 'BUSPARTNER = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'INSOBJECTPARTNER' AND gv_objpart EQ c_x AND ls_source NS 'DATA' AND ls_source NS 'DATAX' AND gs_code NS 'OORRX' AND ls_source NS 'CORR' AND ls_source NS 'INSOBJECTPARTNERX'
                           AND ls_source NS 'LOCKX' AND ls_source NS 'LOCK'   AND ls_source NS 'ASSGNX' AND ls_source NS 'ASSGN'
                            AND ls_source NS 'BROKERX' AND ls_source NS 'BROKER' AND ls_source NS 'PAYX' AND ls_source NS 'PAY'
      AND ls_source NS 'INSOBJNUMBER'  AND ls_source NS 'WHV' AND ls_source NS 'WHVX'.
      CONCATENATE 'INSOBJECTPARTNER = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'INSOBJECTPARTNERX'  AND gv_objpartx EQ c_x  AND ls_source NS 'DATA' AND ls_source NS 'DATAX' AND gs_code NS 'OORRX' AND ls_source NS 'CORR'
               AND ls_source NS 'LOCKX' AND ls_source NS 'LOCK'   AND ls_source NS 'ASSGNX' AND ls_source NS 'ASSGN'
       AND ls_source NS 'BROKERX' AND ls_source NS 'BROKER'
      AND ls_source NS 'PAYX' AND ls_source NS 'PAY' AND ls_source NS 'INSOBJNUMBER'
      AND ls_source NS 'WHV' AND ls_source NS 'WHVX'.
      CONCATENATE 'INSOBJECTPARTNERX = ' param3 INTO wa_code1 SEPARATED BY space.

    ENDIF.
*
    IF ls_source CS 'CURRENCY' AND ls_source NS 'CALL'.
      CONCATENATE 'CURRENCY  = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'AMOUNT_INTERNAL'.
      CONCATENATE 'AMOUNT_INTERNAL = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'AMOUNT_EXTERNAL'.
      CONCATENATE 'AMOUNT_EXTERNAL = ' param3 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'I_BORIDENT'.
      CONCATENATE 'I_BORIDENT = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.

    IF ls_source CS 'ET_QMEL'.
      CONCATENATE 'ET_QMEL = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'E_ORIGIN'.
      CONCATENATE 'E_ORIGIN = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'E_ARC_KEY'.
      CONCATENATE 'E_ARC_KEY = ' param3 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'E_ORIGIN_TAB'.
      CONCATENATE 'E_ORIGIN_TAB = ' param4 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'T_RELATIONS'.
      CONCATENATE 'T_RELATIONS = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'FIELDNAME' AND ls_source NS 'S'.
      CONCATENATE 'FIELDNAME = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'TABNAME'.
      CONCATENATE 'TABNAME = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.

    IF ls_source CS 'RETURNCODE'.
      CONCATENATE 'RETURNCODE = ' param4 INTO wa_code1 SEPARATED BY space.

    ENDIF.

    IF ls_source CS 'ACCOUNTDATA'.
      CONCATENATE 'ACCOUNTDATA = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'ACCOUNTDATA' AND gv_act_det EQ 'X'.
      CONCATENATE 'ACCOUNTDATA = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'ACCOUNTDATAX'.
      CONCATENATE 'ACCOUNTDATAX = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF ls_source CS 'ACCOUNTPARTNER' AND ls_source NS 'PARTNERX' AND ls_source NS 'CORR' AND ls_source NS 'CORRX'
      AND ls_source NS 'LOCK' AND ls_source NS 'CHGDISC' AND ls_source NS 'CHGDISCX'.
      CONCATENATE 'ACCOUNTPARTNER = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF ls_source CS 'ACCOUNTPARTNER' AND gv_act_det EQ 'X' AND ls_source NS 'CORR' AND ls_source NS 'LOCK' AND ls_source NS 'CHGDISC'.
      CONCATENATE 'ACCOUNTPARTNER = ' param3 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'ACCOUNTPARTNERX' AND ls_source NS 'CORR' AND ls_source NS 'CORRX'
         AND ls_source NS 'LOCK' AND ls_source NS 'CHGDISC' AND ls_source NS 'CHGDISCX'.
      CONCATENATE 'ACCOUNTPARTNERX = ' param4 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'RETURN'  AND ls_source NS 'TABLE' AND ls_source NS 'CODE' AND ls_source NS 'TAB'.
      IF param5 IS NOT INITIAL.
        CONCATENATE 'RETURN = ' param5 INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF  ls_source CS 'ACCOUNTNUMBER' .
      CONCATENATE 'ACCOUNTNUMBER = ' param6 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'I_TFK047L' .
      CONCATENATE 'I_TFK047L = ' param INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'T_FKKMAZE' .
      CONCATENATE 'T_FKKMAZE = ' param2 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'T_FKKMAKT' .
      CONCATENATE 'T_FKKMAKT = ' param3 INTO wa_code1 SEPARATED BY space.

    ENDIF.
    IF  ls_source CS 'T_FKKOP' .
      CONCATENATE 'T_FKKOP = ' param4 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'T_FIMSG' .
      CONCATENATE 'T_FIMSG = ' param5 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'C_FKKMAKO' .
      CONCATENATE 'C_FKKMAKO = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'IV_PARTNER' AND ls_source NS 'IV_PARTNERROLECATEGORY' AND ls_source NS 'IV_PARTNERROLE' AND ls_source NS 'IV_PARTNER_GUID'.
      CONCATENATE 'IV_PARTNER = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'IV_PARTNER_GUID' .
      CONCATENATE 'IV_PARTNER_GUID = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'IV_PARTNERROLE' AND ls_source NS 'CATEGORY' .
      CONCATENATE 'IV_PARTNERROLE = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'ET_PARTNERROLES' .
      CONCATENATE 'ET_PARTNERROLE = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'ET_RETURN' .
      CONCATENATE 'ET_RETURN = ' param4 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'I_ACTION' .
      CONCATENATE 'I_ATION = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'I_REFDOC' .
      CONCATENATE 'I_REFDOC = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'I_EBELN' .
      CONCATENATE 'I_EBELN = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'I_EBELP' .
      CONCATENATE 'I_EBELP  = ' param4 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'I_MBLNR' .
      CONCATENATE 'I_MBLNR = ' param5 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'I_MJAHR' .
      CONCATENATE 'I_MJAHR = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'IS_HEADER' .
      CONCATENATE 'IS_HEADER = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'EX_ERROR' .
      CONCATENATE 'EX_ERROR = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'TEXT_QUESTION' .
      IF param5 IS NOT INITIAL AND param IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF '''' IN param5 WITH space.
        REPLACE ALL OCCURRENCES OF '''' IN param WITH space.
        CONCATENATE '''' param5 param '''' INTO DATA(lv_qstr).

        CONCATENATE 'TEXT_QUESTION = ' lv_qstr INTO wa_code1 SEPARATED BY space.
      ELSE.
        CONCATENATE 'TEXT_QUESTION = ' param INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF  ls_source CS 'TITLEBAR' .
      CONCATENATE 'TITLEBAR = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'POPUP_TITLE' .
      CONCATENATE 'POPUP_TITLE = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'TEXT_BUTTON_1' .
      CONCATENATE 'TEXT_BUTTON_1 = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'TEXT_BUTTON_2' .
      CONCATENATE 'TEXT_BUTTON_2 = ' param4 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'DISPLAY_CANCEL_BUTTON'.

      CONCATENATE 'DISPlAY_CANCEL_BUTTON = ' '''X''' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'ANSWER' .
      CONCATENATE 'ANSWER = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.
*   *************************************************************
*   New  BAPI_INSPOPER_GETDETAIL  Obsolete BAPI_INSPCHAR_GETREQUIREMENTS
*  ***************************************************************

    IF  ls_source CS 'INSPLOT'  AND ls_source NS 'CALL'..
      CONCATENATE 'INSPLOT = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'INSPOPER'  AND ls_source NS 'CALL'..
      CONCATENATE 'INSPOPER = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'DATA' AND ls_source NS 'INSPPOINTDATA' AND ls_source NS 'NO'   AND ls_source NS 'PROVIDER'
      AND ls_source NS 'TAB' AND ls_source NS 'BAD'  AND ls_source NS 'ACCOUNT' AND gv_objc IS INITIAL.
      CONCATENATE 'DATA = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'INSPPOINTDATA' .
      CONCATENATE 'INSPPOINTDATA = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.


*    *********************
*    BAPI_XBP_JOB_SPOOLLIST_READ
*    ********************8

    IF  ls_source CS 'JOBNAME' .
      CONCATENATE 'JOBNAME = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'JOBCOUNT'  .
      CONCATENATE 'JOBCOUNT = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'EXTERNAL_USER_NAME' .
      CONCATENATE 'EXTERNAL_USER_NAME = ' param3 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'STEP_NUMBER'  AND ls_source NS 'POPUP'.
      CONCATENATE 'STEP_NUMBER = ' param4 INTO wa_code1 SEPARATED BY space.
    ENDIF.
    IF  ls_source CS 'SPOOL_LIST' AND ls_source NS 'PLAIN' .
      CONCATENATE 'SPOOL_LIST = ' param6 INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'MATERIAL' AND ls_source NS '_EVG' AND ls_source  NS '_LONG'.
      CONCATENATE 'MATERIAL = ' param INTO wa_code1 SEPARATED BY space.
    ENDIF.

    IF  ls_source CS 'BATCH' AND ls_source NS 'CALL'  AND  ls_source NS 'S' AND  ls_source NS 'ATTRIBUTESX' AND ls_source NS 'NO'
                   AND ls_source  NS 'STATUS' AND ls_source NS 'STATUSX' AND ls_source NS 'STORAGELOCATION' AND ls_source NS 'CONTROLFIELDS'..
      CONCATENATE 'BATCH = ' param2 INTO wa_code1 SEPARATED BY space.
    ENDIF.

*    IF  ls_source CS 'INSPREQUIREMENTS' .
*      CONCATENATE 'INSPLOT = ' param INTO wa_code1 SEPARATED BY space.
*    ENDIF.
*    IF  ls_source CS 'RETURN' .
*      CONCATENATE 'RETURN = ' param5 INTO wa_code1 SEPARATED BY space.
*    ENDIF.

****************************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .          " vinod

***************************************************************************

    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
***********************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  .      " vinod
***********************************************************

  p_tabix = p_tabix + 1.
*  INSERT WA_CODE1 INTO  IT_CODE ."INDEX P_TABIX .
*  P_TABIX = P_TABIX + 1.
*  INSERT WA_CODE1 INTO  IT_CODE  ."INDEX P_TABIX.
*  CLEAR LV_TABIX.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab.
  CLEAR : param,param2,param3,param4,param5,param6.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_GET_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM popup_get_value .

  wa_dip-count_no = lv_count + 1.
  wa_dip-obsolete = 'POPUP_TO_GET_VALUE'.
  wa_dip-zreplace =  'POPUP_GET_VALUES'.
  wa_dip-stfm = 'FUNCTION MOUDLE REPLACED'.
  wa_dip-zprogram = ls_report-name.
  APPEND wa_dip TO it_dip.
  INSERT /vshaneya/zprg FROM wa_dip.
  CLEAR wa_dip.
  CONDENSE wa_code.
  CONCATENATE '*' wa_code INTO wa_code.
  CONDENSE wa_code.
  MODIFY it_code FROM wa_code INDEX l_tabix.
  CLEAR wa_code.
  PERFORM replace_fnmodule USING 'POPUP_GET_VALUES' l_tabix.

ENDFORM.
FORM user_command USING pv_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.


  CASE pv_ucomm.
    WHEN 'DWLD'  .
      PERFORM download_file.
  ENDCASE.


  READ TABLE it_dip INTO wa_dip INDEX rs_selfield-tabindex.
  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'
      object_name         = wa_dip-zprogram
      object_type         = 'PROG'
*     ENCLOSING_OBJECT    =
      position            = wa_dip-line
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
**&---------------------------------------------------------------------*
*& Form GET_FM_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_TAB  text
*&---------------------------------------------------------------------*
FORM get_fm_parameters  USING    p_tab TYPE sy-tabix
                                 p_fmodule TYPE string
                                 p_code TYPE string
                                 CHANGING p_param  TYPE string
                                          p_param2   TYPE string
                                          p_param3 TYPE string
                                          p_param4 TYPE string
                                          p_param5 TYPE string
                                          p_param6   TYPE string. .


  DATA lv_st1 TYPE c LENGTH 30.
  DATA lv_st2 TYPE c LENGTH 30.
  IF p_fmodule  CS 'WS_UPLOAD' .
    gv_imp = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.

      IF gs_code CS 'FILENAME'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'FILETYPE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'HEADLEN'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'DATA_TAB'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR GT_CODE[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

*    GT_CODE[] = IT_CODE[].

  ELSEIF p_fmodule  CS 'WS_DOWNLOAD' .
    gv_imp = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.


      IF gs_code CS 'FILENAME'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'FILETYPE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'DATA_TAB'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.

    ENDLOOP.
  ELSEIF  p_fmodule  CS 'COPCA_SET_CO_OBJECTS_FOR_AM' .
    CLEAR :gv_accit,gv_amco.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'T_ACCIT'  AND gv_accit IS INITIAL.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .

        p_param = lv_st2.

        gv_accit = c_x.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'T_ACCIT_AMCO' AND gv_amco IS  INITIAL AND  gv_accit IS NOT INITIAL.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
        gv_amco = c_x.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'FS_MAP_BAPI_CUREXT_CONV_TO_EXT' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'CURRENCY'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'AMOUNT_INTERNAL'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'AMOUNT_EXTERNAL'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param3 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.

    ENDLOOP.

  ELSEIF  p_fmodule  CS 'BAPI_CD_INSOBJ_GETDETAIL' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'INSOBJNUMBER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'INSOBJECTPARTNER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
        gv_ins_gt = c_x.
      ENDIF.

    ENDLOOP.

  ELSEIF  p_fmodule  CS 'ASH_PM_QMEL_RELATIONS_GET' OR p_fmodule  CS 'ASH_SM_QMEL_RELATIONS_GET' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'I_BORIDENT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'T_RELATIONS'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.

    ENDLOOP.
  ELSEIF  p_fmodule  CS 'HELP_VALUES_GET' .
    gv_tab = c_x.
    gv_imp = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'FIELDNAME'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'TABNAME'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        param2 = lv_st2.
*        CLEAR gt_code[].
        CLEAR : lv_st1,lv_st2.
      ENDIF.

    ENDLOOP.
  ELSEIF  p_fmodule  CS 'BAPI_CD_ACCOUNT_CREATE' ."or p_fmodule  CS 'ASH_SM_QMEL_RELATIONS_GET' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'ACCOUNTDATA'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'ACCOUNTDATAX'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ACCOUNTPARTNER' AND gs_code NS 'ACCOUNTPARTNERX' AND gs_code NS 'CORR' AND gs_code NS 'CORRX' AND gs_code NS 'LOCK'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'ACCOUNTPARTNERX'  AND gs_code NS 'CORR' AND gs_code NS 'CORRX' AND gs_code NS 'LOCK'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF  gs_code CS 'RETURN' AND gs_code NS 'RETURNTABLE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF  gs_code CS 'ACCOUNTNUMBER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'FMCA_DUNNING_PF_NO_DET_0350' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'I_TFK047L'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'T_FKKMAZE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'T_FKKMAKT' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'T_FKKOP'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'T_FIMSG'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'C_FKKMAKO'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'BUPA_ROLES_GET' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'IV_PARTNER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'IV_PARTNER_GUID'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ET_PARTNERROLES' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ET_RETURN'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'MIGO_GR_CREATE' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'I_ACTION'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'I_REFDOC'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'I_EBELN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'I_EBELP'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'I_MBLNR'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'I_MJAHR'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'ISCD_INSOBJECT_MAINTAIN' .
    gv_tab = c_x.

    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'I_HEADER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'E_XERROR'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

    ENDLOOP.

  ELSEIF  p_fmodule  CS 'POPUP_TO_DECIDE' .
    gv_tab = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'TEXTLINE1'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TITLE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TEXT_OPTION1'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TEXT_OPTION2' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'CANCEL_DISPLAY'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ANSWER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
    p_param3 = '''YES'''.
    p_param4 = '''NO'''.
  ELSEIF  p_fmodule  CS 'POPUP_TO_CONFIRM_WITH_VALUE' .
    gv_tab = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'OBJECTVALUE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TITLE'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TEXT_BEFORE'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.
        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'ANSWER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
    p_param3 = '''YES'''.
    p_param4 = '''NO'''.


  ELSEIF  p_fmodule  CS 'ASH_PM_QMEL_DISPLAY' OR p_fmodule  CS 'ASH_SM_QMEL_DISPLAY'.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      IF gs_code CS 'I_BORIDENT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.


    ENDLOOP.
  ELSEIF  p_fmodule  CS 'ASH_SM_QMEL_ORIGIN_GET' OR p_fmodule  CS 'ASH_PM_QMEL_ORIGIN_GET' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'I_BORIDENT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      DATA gv_or TYPE c.
      IF gs_code CS 'E_ORIGIN' AND gs_code NS 'ORIGIN_TAB' AND gv_or EQ space.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        gv_or = 'X'.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'E_ARC_KEY'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'E_ORIGIN_TAB' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.


      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'BUPA_ROLE_ADD' OR p_fmodule CS 'BUPA_ROLE_EXISTENCE_CHECK'.

    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'IV_PARTNER' AND gs_code NS 'CATEGORY' AND gs_code NS 'IV_PARTNERROLE' AND gs_code NS 'GUID'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'IV_PARTNER_GUID'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'IV_PARTNERROLE' AND gs_code NS 'CATEGORY' AND gs_code NS 'GUID'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ET_RETURN'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'BAPI_CD_ACCOUNT_CHANGE' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'ACCOUNTDATA' AND gs_code NS 'X'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ACCOUNTDATAX'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'ACCOUNTPARTNER' AND  gs_code NS 'ACCOUNTPARTNERX' AND gs_code NS 'CORR' AND gs_code NS 'CORRX' AND gs_code NS 'LOCK'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ACCOUNTPARTNERX' AND  wa_code  NS 'ACCOUNTPARTNER' AND gs_code NS 'CORR' AND gs_code NS 'CORRX' AND gs_code NS 'LOCK'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'RETURN'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSOBJ_NUMBER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.
        gv_c = 'X'.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.


  ELSEIF  p_fmodule  CS 'BAPI_CD_ACCOUNT_GETDETAIL' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'BUSPARTNER' AND gs_code NS 'EXT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ACCOUNTDATA'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.
        gv_act_det = 'X'.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ACCOUNTPARTNER' AND  gs_code NS 'CORR' AND gs_code NS 'LOCK'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        gv_act_det = 'X'.
        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'RETURN'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSOBJ_NUMBER'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.
        gv_acct = 'X'.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'BAPI_INSPCHAR_GETREQUIREMENTS' .
    gv_tab = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'INSPLOT'  AND gs_code NS 'OPER' AND gs_code NS 'CHAR'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPOPER' AND gs_code NS 'LOT' AND gs_code NS 'CHAR'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPCHAR' AND gs_code NS 'OPER' AND gs_code NS 'LOT'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'REQUIREMENTS'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'RETURN'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'BAPI_INSPCHAR_SETRESULT' .
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'INSPLOT'  AND gs_code NS 'OPER' AND gs_code NS 'CHAR'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPOPER' AND gs_code NS 'LOT' AND gs_code NS 'CHAR'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPCHAR' AND gs_code NS 'OPER' AND gs_code NS 'LOT'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'RETURN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'BAPI_INSPPOINT_CHANGE' .
    gv_tab = 'X'.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'INSPLOT'  AND gs_code NS 'OPER' AND gs_code NS 'CHAR'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPOPER' AND gs_code NS 'LOT' AND gs_code NS 'CHAR'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSPCHAR' AND gs_code NS 'OPER' AND gs_code NS 'LOT'..
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'RETURN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'DATA'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.

  ELSEIF  p_fmodule  CS 'BAPI_XBP_JOB_SPOOLLIST_READ' .

    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'JOBNAME'  AND gs_code NS 'COUNT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'JOBCOUNT' AND gs_code NS 'NAME' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'EXTERNAL_USER_NAME' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'STEP_NUMBER' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'RETURN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'SPOOL_LIST'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param6 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.





  ELSEIF  p_fmodule  CS '/ISDFPS/BAPI_BATCH_SAVE_REPLIC' .

    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'MATERIAL'  AND gs_code NS '_EVG' AND gs_code NS '_LONG'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'BATCH' AND gs_code NS 'ATTRIBUTES' AND gs_code NS 'ATTRIBUTESX'
                   AND gs_code  NS 'STATUS' AND gs_code NS 'STATUSX' AND gs_code NS 'STORAGELOCATION' AND gs_code NS 'CONTROLFIELDS'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'RETURN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'WS_FILENAME_GET' .
    gv_exp = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'DEF_FILENAME' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'FILENAME' AND gs_code NE 'DEF_' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'ASH_PM_QMEL_READ' .

    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'I_BORIDENT' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ET_QMEL' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'BAPI_CD_INSOBJ_CHANGE' .

    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'INSOBJNUMBER' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.
        gv_objc = c_x.
        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'INSOBJECTPARTNER'  AND gs_code NS 'DATA' AND gs_code NS 'OORRX' AND gs_code NS 'CORR' AND gs_code NS 'INSOBJECTPARTNERX'
                           AND gs_code NS 'LOCKX' AND gs_code NS 'LOCK'   AND gs_code NS 'ASSGNX' AND gs_code NS 'ASSGN'
                            AND gs_code NS 'BROKERX' AND gs_code NS 'BROKER' AND gs_code NS 'PAYX' AND gs_code NS 'PAY' AND gs_code NS 'INSOBJNUMBER' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
        gv_objpart = c_x.
      ENDIF.
      IF gs_code CS 'INSOBJECTPARTNERX'  AND gs_code NS 'DATA' AND gs_code NS 'OORRX' AND gs_code NS 'CORR'
                           AND gs_code NS 'LOCKX' AND gs_code NS 'LOCK'   AND gs_code NS 'ASSGNX' AND gs_code NS 'ASSGN'
                            AND gs_code NS 'BROKERX' AND gs_code NS 'BROKER'   AND gs_code NS 'PAYX' AND gs_code NS 'PAY' AND gs_code NS 'INSOBJNUMBER'.

        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.
        gv_objpartx = c_x.
        CLEAR : lv_st1,lv_st2.
      ENDIF.

      IF gs_code CS 'RETURN' .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param5 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ELSEIF  p_fmodule  CS 'POPUP_TO_GET_VALUE' .
    gv_exp = c_x.
    LOOP AT gt_code INTO gs_code FROM p_tab.
      SHIFT gs_code LEFT DELETING LEADING space.
      IF gs_code CS 'FIELDNAME'  AND gs_code NS 'NOT'.
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TABNAME'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param2 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'TITLE'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param3 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
      IF gs_code CS 'ANSWER'  .
        SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .
        p_param4 = lv_st2.

        CLEAR : lv_st1,lv_st2.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR : lv_st2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRETTY_PRINTER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_IT_CODE  text
*&---------------------------------------------------------------------*
FORM pretty_printer  CHANGING p_code TYPE string_table.

  CALL FUNCTION 'PRETTY_PRINTER'
    EXPORTING
      inctoo = space
    TABLES
      ntext  = p_code
      otext  = p_code.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM update_log  USING   p_count TYPE int4
                          p_obsolete TYPE string
                          p_replace TYPE string
                          p_statem  TYPE string
                          p_prog    TYPE rs38m-programm
                          p_line TYPE sy-tabix.


  wa_dip-count_no = p_count.
  wa_dip-obsolete = p_obsolete.
  wa_dip-zreplace =  p_replace.
  wa_dip-stfm = p_statem.
  wa_dip-zprogram = p_prog.
  wa_dip-line = p_line.
  APPEND wa_dip TO it_dip.
ENDFORM.
**&---------------------------------------------------------------------*
*& Form GET_FM_PARAMETERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_TAB  text
*&---------------------------------------------------------------------*
FORM get_old_fm_par  USING    p_tab TYPE sy-tabix
                                 CHANGING it_old_par TYPE ty_t_old_par.


  DATA lv_st1 TYPE c LENGTH 30.
  DATA lv_st2 TYPE c LENGTH 30.
  DATA: len    TYPE i,
        lv_dot TYPE c.

  REFRESH: it_old_par .
  LOOP AT gt_code INTO gs_code FROM p_tab.
    SPLIT gs_code AT '=' INTO lv_st1 lv_st2 .

    IF gs_code NS 'EXPORTING' AND gs_code NS 'IMPORTING' AND gs_code NS 'TABLES' AND gs_code NS 'EXCEPTIONS' AND gs_code NS '*'
      AND gs_code NS 'CALL0'.
      SPLIT gs_code AT '=' INTO lv_st1 lv_st2.
      CONDENSE: lv_st1, lv_st2.
      TRANSLATE lv_st1 TO UPPER CASE.
      wa_old_par-par = lv_st1.
      IF lv_st2 IS NOT INITIAL
      AND lv_st2 CA '.'.
        len = strlen( lv_st2 ).
        len = len - 1.
        lv_st2 = lv_st2+0(len).
      ENDIF.
      wa_old_par-val = lv_st2.
      APPEND wa_old_par TO it_old_par.
      CLEAR wa_old_par.
    ENDIF.
    DELETE it_old_par WHERE val IS INITIAL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REPLACE_FNMODULE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM replace_fnmodule_class USING p_class TYPE seoclsname
      p_method TYPE abap_methname
      it_param TYPE abap_parmdescr_tab
  it_exec TYPE abap_excpdescr_tab
  it_seosubcodf TYPE /vshaneya/tt_seosubcodf
  p_tabix TYPE sy-tabix
  it_old_par TYPE ty_t_old_par.

  FIELD-SYMBOLS: <l_fs_param>      TYPE abap_parmdescr,
                 <l_fs_exec>       TYPE abap_excpdescr,
                 <l_fs_seosubcodf> TYPE seosubcodf,
                 <l_fs_old_par>    TYPE ty_old_par,
                 <lwa_code>        TYPE string.
  DATA: lt_source TYPE rswsourcet,l_v_index TYPE string,
        ls_source TYPE string.

***Changes by avi
  IF p_method EQ 'GUI_DOWNLOAD'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>gui_download'.
  ELSEIF p_method EQ 'GUI_UPLOAD'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>gui_upload'.
  ELSEIF p_method EQ 'CLIPBOARD_EXPORT'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>clipboard_export'.
  ELSEIF p_method EQ 'CLIPBOARD_IMPORT'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>clipboard_import'.
  ELSEIF p_method EQ 'REGISTRY_GET_VALUE'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>registry_get_value'.
  ELSEIF p_method EQ 'FILE_GET_ATTRIBUTES'.
    ls_source = 'CALL METHOD cl_gui_frontend_services=>FILE_GET_ATTRIBUTES'.
  ENDIF.
***
  APPEND ls_source TO lt_source.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'I'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'EXPORTING' INTO     ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'I'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND ( <l_fs_seosubcodf>-parvalue IS INITIAL OR <l_fs_seosubcodf>-parvalue EQ 'SPACE' )
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_exp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    IF p_method EQ 'REGISTRY_GET_VALUE'. " as it is compulsory
      gv_exp = abap_true.
    ENDIF.
  ENDIF.


  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'E'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'IMPORTING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'E'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1 AND <l_fs_param>-is_optional IS INITIAL.
          gv_imp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.

      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'C'.
  IF sy-subrc EQ 0.
    CONCATENATE '*'      'CHANGING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'C'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_chg = abap_true.
        ENDIF.

        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_exec ASSIGNING <l_fs_exec> INDEX 1.
  IF sy-subrc EQ 0 AND <l_fs_exec> IS ASSIGNED.
    ls_source = 'EXCEPTIONS'.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_exec ASSIGNING <l_fs_exec>.
      l_v_index = sy-tabix.
      CONCATENATE  <l_fs_exec>-name '=' l_v_index INTO ls_source SEPARATED BY space.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.
*******************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX. " vinod
*********************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.
******************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .           " vinod
**************************************************************


  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS NOT INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'CHANGING' AND gv_chg IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.

    IF ls_source CS 'BIN_FILESIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'BIN_FILESIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'BIN_FILESIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'CODEPAGE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'CODEPAGE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'CODEPAGE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    DATA: lv_present TYPE xfeld.
    DATA: lwa_code TYPE string.
    DATA: lv_data  TYPE c,
          lv_tabix TYPE sy-tabix.
    IF ls_source CS 'FILENAME' AND ls_source NS 'DEF_'  AND ls_source NS 'CALL'.
***Changes by avi
      IF p_method EQ 'GUI_UPLOAD'
      OR p_method EQ 'GUI_DOWNLOAD'.
        READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILENAME'.
        IF sy-subrc EQ 0.
***Check if data type is same
          DESCRIBE FIELD <l_fs_old_par>-val TYPE lv_data.
          CALL METHOD /vshaneya/cl_fm_remediation=>check_data_type
            EXPORTING
              i_class       = p_class
              i_method      = p_method
              i_sconame     = 'FILENAME'
              i_value       = <l_fs_old_par>-val
              i_data_type   = lv_data
              it_seosubcodf = it_seosubcodf
            IMPORTING
              e_present     = lv_present.
          IF lv_present EQ abap_true.
            CONCATENATE 'FILENAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
          ELSE.
            LOOP AT it_code INTO lwa_code.
              IF lwa_code CS 'DATA:'.
                EXIT.
              ENDIF.
            ENDLOOP.
            CONCATENATE `DATA: lv_filename type string.` `"Changes by Haneya Tool` INTO lwa_code.
            INSERT lwa_code INTO it_code INDEX sy-tabix.
            CLEAR: lwa_code.
          ENDIF.
        ENDIF.
      ELSEIF p_method EQ 'FILE_GET_ATTRIBUTES'.
        READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILE'.
        IF sy-subrc EQ 0.
***Check if data type is same
*          DESCRIBE FIELD <l_fs_old_par>-val TYPE lv_data.
*          CALL METHOD /vshaneya/cl_fm_remediation=>check_data_type
*            EXPORTING
*              i_class       = p_class
*              i_method      = p_method
*              i_sconame     = 'FILENAME'
*              i_value       = <l_fs_old_par>-val
*              i_data_type   = lv_data
*              it_seosubcodf = it_seosubcodf
*            IMPORTING
*              e_present     = lv_present.
*          IF lv_present EQ abap_true.
          CONCATENATE 'FILENAME = ' 'lv_file' INTO wa_code1 SEPARATED BY space.
*          ELSE.
***
          LOOP AT it_code INTO lwa_code.
            IF lwa_code CS 'lv_file'.
              lv_tabix = sy-tabix.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF lv_tabix IS NOT INITIAL.
            IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
              IF lwa_code CS ','.
                CONCATENATE `DATA: lv_file type string,` `"Changes by Haneya Tool` INTO lwa_code.
              ELSE.
                CONCATENATE `DATA: lv_file type string.` `"Changes by Haneya Tool` INTO lwa_code.
              ENDIF.
            ELSE.
              IF lwa_code CS ','.
                CONCATENATE `lv_file type string,` `"Changes by Haneya Tool` INTO lwa_code.
              ELSE.
                CONCATENATE `lv_file type string.` `"Changes by Haneya Tool` INTO lwa_code.
              ENDIF.
            ENDIF.
            READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
            IF <lwa_code> IS ASSIGNED.
              CONCATENATE '*' <lwa_code> INTO <lwa_code>.
            ENDIF.
            UNASSIGN <lwa_code>.
            INSERT lwa_code INTO it_code INDEX lv_tabix.
            CLEAR: lwa_code.
          ELSEIF lv_tabix IS INITIAL.
            LOOP AT it_code INTO lwa_code.
              IF lwa_code CS 'DATA:'.
                EXIT.
              ENDIF.
            ENDLOOP.
            CONCATENATE `DATA: lv_file type string.` `"Changes by Haneya Tool` INTO lwa_code.
            INSERT lwa_code INTO it_code INDEX sy-tabix.
            CLEAR: lwa_code.
          ENDIF.
***
*          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILENAME'.
        IF sy-subrc EQ 0.
          CONCATENATE 'FILENAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILETYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILETYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILETYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DAT_MODE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'MODE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DAT_MODE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT_MASK'.

      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECTMASK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT_MASK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'NO_AUTH_CHECK'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'NO_AUTH_CHECK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'NO_AUTH_CHECK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILELENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILELENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILELENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DATA_TAB'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'DATA_TAB'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DATA_TAB = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
***Changes by avi
    IF ls_source CS 'KEY' AND ( p_method EQ 'REGISTRY_GET_VALUE').
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'KEY'.
      IF sy-subrc EQ 0.
***Check if data type is same
        CALL METHOD /vshaneya/cl_fm_remediation=>check_data_type
          EXPORTING
            i_class       = p_class
            i_method      = p_method
            i_sconame     = 'KEY'
            i_value       = <l_fs_old_par>-val
            it_seosubcodf = it_seosubcodf
          IMPORTING
            e_present     = lv_present.
        IF lv_present EQ abap_true.
          CONCATENATE 'KEY = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
        ELSE.
          LOOP AT it_code INTO lwa_code.
            IF lwa_code CS 'DATA:'.
              EXIT.
            ENDIF.
          ENDLOOP.
          CONCATENATE `DATA: lv_key type string value ` `'` <l_fs_old_par>-val `'"Changes by Haneya Tool` INTO lwa_code.
          INSERT lwa_code INTO it_code INDEX sy-tabix.
          CLEAR: lwa_code.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_source CS ' VALUE =' AND ( p_method EQ 'REGISTRY_GET_VALUE').
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'SECTION'.
      IF sy-subrc EQ 0.
***Check if data type is same
        CALL METHOD /vshaneya/cl_fm_remediation=>check_data_type
          EXPORTING
            i_class       = p_class
            i_method      = p_method
            i_sconame     = 'VALUE'
            it_seosubcodf = it_seosubcodf
            i_value       = <l_fs_old_par>-val
          IMPORTING
            e_present     = lv_present.
        IF lv_present EQ abap_true.
          CONCATENATE 'VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
        ELSE.
          LOOP AT it_code INTO lwa_code.
            IF lwa_code CS 'DATA:'.
              EXIT.
            ENDIF.
          ENDLOOP.
          CONCATENATE `DATA: lv_value type string value ` `'` <l_fs_old_par>-val `'"Changes by Haneya Tool` INTO lwa_code.
          INSERT lwa_code INTO it_code INDEX sy-tabix.
          CLEAR: lwa_code.
        ENDIF.
      ENDIF.
    ENDIF.
    IF ls_source CS 'REG_VALUE' AND ( p_method EQ 'REGISTRY_GET_VALUE').
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'VALUE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'REG_VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'ROOT' AND ( p_method EQ 'REGISTRY_GET_VALUE').
      wa_code1 = 'ROOT = 1'.
    ENDIF.
    IF ls_source CS 'DATA' AND ( p_method EQ 'CLIPBOARD_EXPORT' OR  p_method EQ 'CLIPBOARD_IMPORT' ).
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'DATA_TAB'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DATA = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'RC' AND p_method EQ 'CLIPBOARD_EXPORT'.
      LOOP AT it_code INTO lwa_code.
        IF lwa_code CS 'DATA:'.
          EXIT.
        ENDIF.
      ENDLOOP.
      lwa_code = 'DATA: lv_rc type sy-subrc.'.
      INSERT lwa_code INTO it_code INDEX sy-tabix.
      CLEAR: lwa_code.
      CONCATENATE 'RC = ' 'lv_rc' INTO wa_code1 SEPARATED BY space.
    ENDIF.
***
    IF ls_source CS 'FIELDNAMES'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FIELDNAMES'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FIELDNAMES = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
******************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .            " vinod
*********************************************************************
    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
*******************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  .        " vinod
********************************************************
  p_tabix = p_tabix + 1.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab,gv_chg.
ENDFORM.
FORM replace_fnmodule_class_icl USING p_class TYPE seoclsname
                                      p_method TYPE abap_methname
                                      it_param TYPE abap_parmdescr_tab
                                      it_exec TYPE abap_excpdescr_tab
                                      it_seosubcodf TYPE /vshaneya/tt_seosubcodf
                                      p_tabix TYPE sy-tabix
                                      it_old_par TYPE ty_t_old_par.

  FIELD-SYMBOLS: <l_fs_param>      TYPE abap_parmdescr,
                 <l_fs_exec>       TYPE abap_excpdescr,
                 <l_fs_seosubcodf> TYPE seosubcodf,
                 <l_fs_old_par>    TYPE ty_old_par.
  DATA: lt_source TYPE rswsourcet,l_v_index TYPE string,
        ls_source TYPE string.

***Changes by avi
  IF p_method EQ 'ICL_PER_TICL003_SELECT'.
    ls_source = 'CALL METHOD CL_ICL_IIF_PS=>ICL_PER_TICL003_SELECT'.
  ENDIF.
***
  APPEND ls_source TO lt_source.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'I'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'EXPORTING' INTO     ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'I'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND ( <l_fs_seosubcodf>-parvalue IS INITIAL OR <l_fs_seosubcodf>-parvalue EQ 'SPACE' )
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_exp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.


  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'E'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'IMPORTING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'E'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_imp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.

      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'C'.
  IF sy-subrc EQ 0.
    CONCATENATE '*'      'CHANGING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'C'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_chg = abap_true.
        ENDIF.

        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_exec ASSIGNING <l_fs_exec> INDEX 1.
  IF sy-subrc EQ 0 AND <l_fs_exec> IS ASSIGNED.
    ls_source = 'EXCEPTIONS'.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_exec ASSIGNING <l_fs_exec>.
      l_v_index = sy-tabix.
      CONCATENATE  <l_fs_exec>-name '=' l_v_index INTO ls_source SEPARATED BY space.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.
******************************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX.     " vinod
***************************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.
***************************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .    " vinod
****************************************************************

  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS NOT INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'CHANGING' AND gv_chg IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.

    IF ls_source CS 'BIN_FILESIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'BIN_FILESIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'BIN_FILESIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'CODEPAGE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'CODEPAGE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'CODEPAGE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    DATA: lv_present TYPE xfeld.
    DATA: lwa_code TYPE string.
    DATA: lv_data TYPE c.
    IF ls_source CS 'FILENAME' AND ls_source NS 'DEF_'  AND ls_source NS 'CALL'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILENAME'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILENAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILETYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILETYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILETYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DAT_MODE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'MODE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DAT_MODE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT_MASK'.

      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECTMASK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT_MASK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'NO_AUTH_CHECK'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'NO_AUTH_CHECK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'NO_AUTH_CHECK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILELENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILELENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILELENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DATA_TAB'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'DATA_TAB'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DATA_TAB = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FIELDNAMES'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FIELDNAMES'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FIELDNAMES = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
****************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .           " vinod
*****************************************************************
    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
*****************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  .
*****************************************************
  p_tabix = p_tabix + 1.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab,gv_chg.
ENDFORM.
FORM replace_fnmodule_class_oo USING p_class TYPE seoclsname
                                      p_method TYPE abap_methname
                                      it_param TYPE abap_parmdescr_tab
                                      it_exec TYPE abap_excpdescr_tab
                                      it_seosubcodf TYPE /vshaneya/tt_seosubcodf
                                      p_tabix TYPE sy-tabix
                                      it_old_par TYPE ty_t_old_par.

  FIELD-SYMBOLS: <l_fs_param>      TYPE abap_parmdescr,
                 <l_fs_exec>       TYPE abap_excpdescr,
                 <l_fs_seosubcodf> TYPE seosubcodf,
                 <l_fs_old_par>    TYPE ty_old_par,
                 <lwa_code>        TYPE string.
  DATA: lt_source      TYPE rswsourcet,l_v_index TYPE string,
        ls_source      TYPE string,
        lwa_code       TYPE string,
        lv_tabix       TYPE sy-tabix,
        lv_ref_present TYPE c.
***Changes by avi
  IF p_method EQ 'GET_INCLUDE_BY_TRKEY'.
    ls_source = 'CALL METHOD CL_OO_INCLUDE_NAMING=>GET_INCLUDE_BY_TRKEY'.
  ELSEIF p_method EQ 'GET_EXTENSION_OF_INCLUDE'.
    ls_source = 'CALL METHOD CL_OO_INCLUDE_NAMING=>GET_EXTENSION_OF_INCLUDE'.
  ELSEIF p_method EQ 'GET_INCLUDE_BY_MTDNAME'.
    ls_source = 'CALL METHOD ref_CLASS_INCL_NAMING->GET_INCLUDE_BY_MTDNAME'.
  ELSEIF p_method EQ 'GET_MTDNAME_BY_INCLUDE'.
    ls_source = 'CALL METHOD ref_CLASS_INCL_NAMING->GET_MTDNAME_BY_INCLUDE'.
  ELSEIF p_method EQ 'GET_INSTANCE_BY_NAME'.
    ls_source = 'CALL METHOD CL_OO_INCLUDE_NAMING=>GET_INSTANCE_BY_NAME'.
  ENDIF.
***
  APPEND ls_source TO lt_source.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'I'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'EXPORTING' INTO     ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'I'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND ( <l_fs_seosubcodf>-parvalue IS INITIAL OR <l_fs_seosubcodf>-parvalue EQ 'SPACE' )
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_exp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.


  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'E'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'IMPORTING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'E'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_imp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.

      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'C'.
  IF sy-subrc EQ 0.
    CONCATENATE '*'      'CHANGING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'C'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_chg = abap_true.
        ENDIF.

        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_exec ASSIGNING <l_fs_exec> INDEX 1.
  IF sy-subrc EQ 0 AND <l_fs_exec> IS ASSIGNED.
    ls_source = 'EXCEPTIONS'.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_exec ASSIGNING <l_fs_exec>.
      l_v_index = sy-tabix.
      CONCATENATE  <l_fs_exec>-name '=' l_v_index INTO ls_source SEPARATED BY space.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.
******************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX.                       " vinod
*********************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.
************************************************************

*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .   " vinod
***************************************************************
  IF p_method EQ 'GET_INSTANCE_BY_NAME'.
    LOOP AT it_code INTO lwa_code.
      IF lwa_code CS 'ref_CIFREF'.
        lv_ref_present = abap_true.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT it_code INTO lwa_code.
      IF lwa_code CS 'ref_CLASS_INCL_NAMING'.
        lv_ref_present = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_method EQ 'GET_INCLUDE_BY_MTDNAME' AND lv_ref_present IS INITIAL.
    p_tabix = p_tabix + 1.
    wa_code1 = 'DATA: ref_CLASS_INCL_NAMING type ref to if_oo_class_incl_naming.'.
**********************************************************************
*    INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .            " vinod
********************************************************
  ELSEIF p_method EQ 'GET_MTDNAME_BY_INCLUDE' AND lv_ref_present IS INITIAL.
    p_tabix = p_tabix + 1.
    wa_code1 = 'DATA: ref_CLASS_INCL_NAMING type ref to if_oo_class_incl_naming.'.
****************************************************************
*    INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .       " vinod
******************************************************************
  ELSEIF p_method EQ 'GET_INSTANCE_BY_NAME' AND lv_ref_present IS INITIAL.
    p_tabix = p_tabix + 1.
    wa_code1 = 'DATA: ref_CIFREF type ref to IF_OO_CLIF_INCL_NAMING.'.
*****************************************************************
*    INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .
****************************************************************

    LOOP AT it_code INTO lwa_code.
      IF lwa_code CS 'lv_name'.
        lv_tabix = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_tabix IS NOT INITIAL.
      IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
        IF lwa_code CS ','.
          CONCATENATE `DATA: lv_name type C,` `"Changes by Haneya Tool` INTO lwa_code.
        ELSE.
          CONCATENATE `DATA: lv_name type C.` `"Changes by Haneya Tool` INTO lwa_code.
        ENDIF.
      ELSE.
        IF lwa_code CS ','.
          CONCATENATE `lv_name type C,` `"Changes by Haneya Tool` INTO lwa_code.
        ELSE.
          CONCATENATE `lv_name type C.` `"Changes by Haneya Tool` INTO lwa_code.
        ENDIF.
      ENDIF.
      READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
      IF <lwa_code> IS ASSIGNED.
        CONCATENATE '*' <lwa_code> INTO <lwa_code>.
      ENDIF.
      UNASSIGN <lwa_code>.
      INSERT lwa_code INTO it_code INDEX lv_tabix.
      CLEAR: lwa_code.
    ELSEIF lv_tabix IS INITIAL.
      p_tabix = p_tabix - 1.
      CONCATENATE `DATA: lv_name type C.` `"Changes by Haneya Tool` INTO wa_code1.
      INSERT wa_code1 INTO  it_code INDEX p_tabix .
    ENDIF.
  ENDIF.


  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS NOT INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'CHANGING' AND gv_chg IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.

    IF ls_source CS 'PGMID'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'PGMID'.
      IF sy-subrc EQ 0.
        CONCATENATE 'PGMID = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'OBJECT ' AND ls_source NS 'NO_OBJECTTYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'OBJECT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'OBJECT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'OBJ_NAME'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'OBJ_NAME'.
      IF sy-subrc EQ 0.
        CONCATENATE 'OBJ_NAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
      LOOP AT it_code INTO lwa_code.
        IF lwa_code CS <l_fs_old_par>-val.
          lv_tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
        IF lwa_code CS ','.
          CONCATENATE `DATA: lv_obj_name type TROBJ_NAME,` `"Changes by Haneya Tool` INTO lwa_code.
        ELSE.
          CONCATENATE `DATA: lv_obj_name type TROBJ_NAME.` `"Changes by Haneya Tool` INTO lwa_code.
        ENDIF.
      ELSE.
        IF lwa_code CS ','.
          CONCATENATE `lv_obj_name type TROBJ_NAME,` `"Changes by Haneya Tool` INTO lwa_code.
        ELSE.
          CONCATENATE `lv_obj_name type TROBJ_NAME.` `"Changes by Haneya Tool` INTO lwa_code.
        ENDIF.
      ENDIF.
      READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
      IF <lwa_code> IS ASSIGNED.
        CONCATENATE '*' <lwa_code> INTO <lwa_code>.
      ENDIF.
      UNASSIGN <lwa_code>.
      INSERT lwa_code INTO it_code INDEX lv_tabix.
      CLEAR: lwa_code.
    ENDIF.

    IF ls_source CS 'MTDNAME' AND ls_source NS 'GET_INCLUDE_BY_MTDNAME' AND ls_source NS 'GET_MTDNAME_BY_INCLUDE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'MTDKEY'.
      IF sy-subrc EQ 0.
        CONCATENATE 'MTDNAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
      LOOP AT it_code INTO lwa_code.
        IF lwa_code CS <l_fs_old_par>-val.
          lv_tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF ( lwa_code CS 'DATA:' OR lwa_code CS 'DATA ' ) AND lwa_code NS 'SEOCPDNAME'.
        IF lwa_code CS ','.
          CONCATENATE `DATA: lv_mtdkey type SEOCPDNAME,` `"Changes by Haneya Tool` INTO lwa_code.
        ELSE.
          CONCATENATE `DATA: lv_mtdkey type SEOCPDNAME.` `"Changes by Haneya Tool` INTO lwa_code.
        ENDIF.
      ELSE.
        IF lwa_code NS 'SEOCPDNAME'.
          IF lwa_code CS ','.
            CONCATENATE `lv_mtdkey type SEOCPDNAME,` `"Changes by Haneya Tool` INTO lwa_code.
          ELSE.
            CONCATENATE `lv_mtdkey type SEOCPDNAME.` `"Changes by Haneya Tool` INTO lwa_code.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
      IF <lwa_code> IS ASSIGNED.
        CONCATENATE '*' <lwa_code> INTO <lwa_code>.
      ENDIF.
      UNASSIGN <lwa_code>.
      INSERT lwa_code INTO it_code INDEX lv_tabix.
      CLEAR: lwa_code.
    ENDIF.

    IF ls_source CS 'PROGNAME ' AND ls_source NS 'PROGNAMES'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'PROGNAME'.
      IF sy-subrc EQ 0.
        CONCATENATE 'PROGNAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'EXTENSION' AND ls_source NS 'GET_EXTENSION_OF_INCLUDE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'EXTENSION'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EXTENSION = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'APPENDAGE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'APPENDAGE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'APPENDAGE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'EXT_PLUS_APP'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'EXT_PLUS_APP'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EXT_PLUS_APP = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    DATA: lv_present TYPE xfeld.
    DATA: lv_data TYPE c.
    IF ls_source CS 'FILENAME' AND ls_source NS 'DEF_'  AND ls_source NS 'CALL'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILENAME'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILENAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'NAME' AND ls_source NS 'GET_INSTANCE_BY_NAME'.
      wa_code1 = 'NAME = lv_name'.
    ENDIF.

    IF ls_source CS 'CIFREF'.
      wa_code1 = 'CIFREF = ref_CIFREF'.
    ENDIF.
************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .          " vinod
***********************************************************

    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  .
  p_tabix = p_tabix + 1.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab,gv_chg.
ENDFORM.
FORM replace_fnmodule_class_brf USING p_class TYPE seoclsname
      p_method TYPE abap_methname
      it_param TYPE abap_parmdescr_tab
  it_exec TYPE abap_excpdescr_tab
  it_seosubcodf TYPE /vshaneya/tt_seosubcodf
  p_tabix TYPE sy-tabix
  it_old_par TYPE ty_t_old_par.

  FIELD-SYMBOLS: <l_fs_param>      TYPE abap_parmdescr,
                 <l_fs_exec>       TYPE abap_excpdescr,
                 <l_fs_seosubcodf> TYPE seosubcodf,
                 <l_fs_old_par>    TYPE ty_old_par,
                 <lwa_code>        TYPE string.
  DATA: lt_source TYPE rswsourcet,l_v_index TYPE string,
        ls_source TYPE string.
  DATA: lv_present TYPE xfeld.
  DATA: lwa_code TYPE string.
  DATA: lv_data        TYPE c,
        lv_tabix       TYPE sy-tabix,
        lv_ref_present TYPE c.

***Changes by avi
  IF p_method EQ 'ICL_EVALUATE_EXPRESSION'.
    ls_source = 'CALL METHOD CL_ICL_BRF_PLUS_SERVICES=>ICL_EVALUATE_EXPRESSION'.
  ELSEIF p_method EQ 'SAVE_ALL'.
    ls_source = 'CALL METHOD CL_USER_DEFAULTS=>SAVE_ALL'.
  ELSEIF p_method EQ 'GET_SINGLE_VALUE'.
    ls_source = 'CALL METHOD ref_CL_USER_DEFAULTS->GET_SINGLE_VALUE'.
  ENDIF.
***
  APPEND ls_source TO lt_source.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'I'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'EXPORTING' INTO     ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'I'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND ( <l_fs_seosubcodf>-parvalue IS INITIAL OR <l_fs_seosubcodf>-parvalue EQ 'SPACE' )
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_exp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.


  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'E'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'IMPORTING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'E'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1 AND <l_fs_param>-is_optional IS INITIAL.
          gv_imp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.

      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    IF p_method EQ 'ICL_EVALUATE_EXPRESSION' OR p_method EQ 'GET_SINGLE_VALUE'. " as it is compulsory
      gv_imp = abap_true.
    ENDIF.
  ENDIF.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'C'.
  IF sy-subrc EQ 0.
    CONCATENATE '*'      'CHANGING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'C'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_chg = abap_true.
        ENDIF.

        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_exec ASSIGNING <l_fs_exec> INDEX 1.
  IF sy-subrc EQ 0 AND <l_fs_exec> IS ASSIGNED.
    ls_source = 'EXCEPTIONS'.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_exec ASSIGNING <l_fs_exec>.
      l_v_index = sy-tabix.
      CONCATENATE  <l_fs_exec>-name '=' l_v_index INTO ls_source SEPARATED BY space.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ELSE.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.
*********************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX.
*******************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.
********************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .
********************************************************
  IF p_method EQ 'GET_SINGLE_VALUE'.
    LOOP AT it_code INTO lwa_code.
      IF lwa_code CS 'ref_CL_USER_DEFAULTS'.
        lv_ref_present = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF p_method EQ 'GET_SINGLE_VALUE' AND lv_ref_present IS INITIAL.
    p_tabix = p_tabix + 1.
    wa_code1 = 'DATA: ref_CL_USER_DEFAULTS type ref to CL_USER_DEFAULTS.'.
******************************************************
*    INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .
******************************************************
*  ELSEIF p_method EQ 'GET_INSTANCE_BY_NAME' AND lv_ref_present IS INITIAL.
*    p_tabix = p_tabix + 1.
*    wa_code1 = 'DATA: ref_CIFREF type ref to IF_OO_CLIF_INCL_NAMING.'.
*    INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
*
*    LOOP AT it_code INTO lwa_code.
*      IF lwa_code CS 'lv_name'.
*        lv_tabix = sy-tabix.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*    IF lv_tabix IS NOT INITIAL.
*      IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
*        IF lwa_code CS ','.
*          CONCATENATE `DATA: lv_name type C,` `"Changes by Haneya Tool` INTO lwa_code.
*        ELSE.
*          CONCATENATE `DATA: lv_name type C.` `"Changes by Haneya Tool` INTO lwa_code.
*        ENDIF.
*      ELSE.
*        IF lwa_code CS ','.
*          CONCATENATE `lv_name type C,` `"Changes by Haneya Tool` INTO lwa_code.
*        ELSE.
*          CONCATENATE `lv_name type C.` `"Changes by Haneya Tool` INTO lwa_code.
*        ENDIF.
*      ENDIF.
*      READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
*      IF <lwa_code> IS ASSIGNED.
*        CONCATENATE '*' <lwa_code> INTO <lwa_code>.
*      ENDIF.
*      UNASSIGN <lwa_code>.
*      INSERT lwa_code INTO it_code INDEX lv_tabix.
*      CLEAR: lwa_code.
*    ELSEIF lv_tabix IS INITIAL.
*      p_tabix = p_tabix - 1.
*      CONCATENATE `DATA: lv_name type C.` `"Changes by Haneya Tool` INTO wa_code1.
*      INSERT wa_code1 INTO  it_code INDEX p_tabix .
*    ENDIF.
  ENDIF.

  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS NOT INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'CHANGING' AND gv_chg IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.

***BRF
    IF ls_source CS 'IV_EXPRESSION'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_REQUEST'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_EXPRESSION = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'IV_RESULT_REQUEST'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_XUSING_BUFFER'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_RESULT_REQUEST = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'IS_ACTUAL'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_ACTUAL'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IS_ACTUAL = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_VALUE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_VALUE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_TYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_TYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_TYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_LENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_LENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_LENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_OUTPUT_LENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_OUTPUTLEN'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_OUTPUT_LENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
        LOOP AT it_code INTO lwa_code.
          IF lwa_code CS <l_fs_old_par>-val.
            lv_tabix = sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_tabix IS NOT INITIAL.
          IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
            IF lwa_code CS ','.
              CONCATENATE `DATA:` <l_fs_old_par>-val ' type BRF_RESULT_OUTPUT_LENGTH,' `"Changes by Haneya Tool` INTO lwa_code.
            ELSE.
              CONCATENATE `DATA:` <l_fs_old_par>-val ' type BRF_RESULT_OUTPUT_LENGTH.' `"Changes by Haneya Tool` INTO lwa_code.
            ENDIF.
          ELSE.
            IF lwa_code CS ','.
              CONCATENATE <l_fs_old_par>-val ' type BRF_RESULT_OUTPUT_LENGTH,' `"Changes by Haneya Tool` INTO lwa_code.
            ELSE.
              CONCATENATE <l_fs_old_par>-val ' type BRF_RESULT_OUTPUT_LENGTH.' `"Changes by Haneya Tool` INTO lwa_code.
            ENDIF.
          ENDIF.
          READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
          IF <lwa_code> IS ASSIGNED.
            CONCATENATE '*' <lwa_code> INTO <lwa_code>.
          ENDIF.
          UNASSIGN <lwa_code>.
          INSERT lwa_code INTO it_code INDEX lv_tabix.
          CLEAR: lwa_code.
        ELSEIF lv_tabix IS INITIAL.
          LOOP AT it_code INTO lwa_code.
            IF lwa_code CS 'DATA:'.
              EXIT.
            ENDIF.
          ENDLOOP.
          CONCATENATE `DATA:` <l_fs_old_par>-val ' type BRF_RESULT_OUTPUT_LENGTH.' `"Changes by Haneya Tool` INTO lwa_code.
          INSERT lwa_code INTO it_code INDEX sy-tabix.
          CLEAR: lwa_code.
        ENDIF.
      ENDIF.
    ENDIF.

***BRF-END

    IF ls_source CS 'BIN_FILESIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'BIN_FILESIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'BIN_FILESIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'CODEPAGE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par  = 'CODEPAGE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'CODEPAGE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'FILETYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILETYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILETYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DAT_MODE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'MODE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DAT_MODE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT_MASK'.

      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECTMASK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT_MASK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'NO_AUTH_CHECK'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'NO_AUTH_CHECK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'NO_AUTH_CHECK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILELENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILELENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILELENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DATA_TAB'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'DATA_TAB'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DATA_TAB = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FIELDNAMES'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FIELDNAMES'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FIELDNAMES = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'IV_FIELD_NAME'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_LFDNR'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_FIELD_NAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'EV_VALUE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_BUFFR_VALUE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
*******************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .
******************************************************************

    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
****************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  . " vinod
******************************************************
  p_tabix = p_tabix + 1.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab,gv_chg.
ENDFORM.
FORM replace_fnmodule_class_scp USING p_class TYPE seoclsname
      p_method TYPE abap_methname
      it_param TYPE abap_parmdescr_tab
  it_exec TYPE abap_excpdescr_tab
  it_seosubcodf TYPE /vshaneya/tt_seosubcodf
  p_tabix TYPE sy-tabix
  it_old_par TYPE ty_t_old_par.

  FIELD-SYMBOLS: <l_fs_param>      TYPE abap_parmdescr,
                 <l_fs_exec>       TYPE abap_excpdescr,
                 <l_fs_seosubcodf> TYPE seosubcodf,
                 <l_fs_old_par>    TYPE ty_old_par,
                 <lwa_code>        TYPE string.
  DATA: lt_source TYPE rswsourcet,l_v_index TYPE string,
        ls_source TYPE string.
  DATA: lv_present TYPE xfeld.
  DATA: lwa_code TYPE string.
  DATA: lv_data        TYPE c,
        lv_tabix       TYPE sy-tabix,
        lv_ref_present TYPE c.

***Changes by avi
  IF p_method EQ 'READ_CODEPAGE'.
    ls_source = 'CALL METHOD CL_SCP_SEGMENT_UTIL=>READ_CODEPAGE'.
  ELSEIF p_method EQ 'CHECK_ALL'.
    ls_source = 'CL_SWF_BND_BINDING=>CHECK_ALL'.
  ENDIF.
***
  APPEND ls_source TO lt_source.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'I'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'EXPORTING' INTO     ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'I'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND ( <l_fs_seosubcodf>-parvalue IS INITIAL OR <l_fs_seosubcodf>-parvalue EQ 'SPACE' )
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_exp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    IF p_method EQ 'READ_CODEPAGE'. " as it is compulsory
      gv_exp = abap_true.
    ENDIF.
  ENDIF.


  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'E'.
  IF sy-subrc EQ 0.
    CONCATENATE '*' 'IMPORTING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'E'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1 AND <l_fs_param>-is_optional IS INITIAL.
          gv_imp = abap_true.
        ENDIF.
        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.

      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    IF p_method EQ 'READ_CODEPAGE'. " as it is compulsory
      gv_imp = abap_true.
    ENDIF.
  ENDIF.

  READ TABLE it_param ASSIGNING <l_fs_param> WITH KEY parm_kind = 'C'.
  IF sy-subrc EQ 0.
    CONCATENATE '*'      'CHANGING' INTO ls_source SEPARATED BY space.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_param ASSIGNING <l_fs_param> WHERE parm_kind = 'C'.
      READ TABLE it_seosubcodf ASSIGNING <l_fs_seosubcodf> WITH KEY clsname = p_class
      cmpname = p_method
      sconame = <l_fs_param>-name.
      IF sy-subrc EQ 0.
        IF <l_fs_seosubcodf>-paroptionl IS INITIAL AND <l_fs_seosubcodf>-parvalue IS INITIAL
          AND <l_fs_seosubcodf>-parpasstyp = 1.
          gv_chg = abap_true.
        ENDIF.

        CONCATENATE '*' <l_fs_param>-name '=' <l_fs_seosubcodf>-parvalue INTO ls_source SEPARATED BY space.
      ELSE.
        CONCATENATE '*' <l_fs_param>-name '=' INTO ls_source SEPARATED BY space.
      ENDIF.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
  ENDIF.

  READ TABLE it_exec ASSIGNING <l_fs_exec> INDEX 1.
  IF sy-subrc EQ 0 AND <l_fs_exec> IS ASSIGNED.
    ls_source = 'EXCEPTIONS'.
    APPEND ls_source TO lt_source.
    CLEAR ls_source.
    LOOP AT it_exec ASSIGNING <l_fs_exec>.
      l_v_index = sy-tabix.
      CONCATENATE  <l_fs_exec>-name '=' l_v_index INTO ls_source SEPARATED BY space.
      APPEND ls_source TO lt_source.
      CLEAR ls_source.
    ENDLOOP.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ELSE.
    ls_source = '.'.
    APPEND ls_source TO lt_source.
  ENDIF.
  p_tabix = p_tabix + 1.
  wa_code1 = ''.
*******************************************************
*  INSERT wa_code1 INTO it_code ."INDEX P_TABIX.
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX.             " vinod
*******************************************************
  p_tabix = p_tabix + 1.
  wa_code1 = '*Begin of change by Haneya Tool'.
**********************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX .  " vinod
****************************************************************

  LOOP AT lt_source  INTO ls_source.

    wa_code1 = ls_source.
    REPLACE ALL OCCURRENCES OF '"' IN wa_code1 WITH '*'.
    IF sy-tabix EQ 1.
      CONCATENATE wa_code1 '' INTO wa_code1 SEPARATED BY space.
    ENDIF.
    CONDENSE ls_source.
    IF ls_source CS 'EXPORTING'  AND gv_exp IS NOT INITIAL..
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'TABLES' AND gv_tab IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'CHANGING' AND gv_chg IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.
    IF ls_source CS 'IMPORTING'  AND gv_imp IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF '*' IN wa_code1 WITH ''.
    ENDIF.

***BRF
    IF ls_source CS 'IV_EXPRESSION'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_REQUEST'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_EXPRESSION = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'IV_RESULT_REQUEST'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_XUSING_BUFFER'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_RESULT_REQUEST = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'IS_ACTUAL'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_ACTUAL'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IS_ACTUAL = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_VALUE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_VALUE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_TYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_TYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_TYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'EV_RESULT_LENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_LENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_RESULT_LENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

***BRF-END

    IF ls_source CS 'BIN_FILESIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'BIN_FILESIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'BIN_FILESIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'FILETYPE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILETYPE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILETYPE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DAT_MODE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'MODE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DAT_MODE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_N_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_N_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_N_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_FORMAT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_FORMAT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_FORMAT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'WK1_T_SIZE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'WK1_T_SIZE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'WK1_T_SIZE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECT'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'COL_SELECT_MASK'.

      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'COL_SELECTMASK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'COL_SELECT_MASK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'NO_AUTH_CHECK'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'NO_AUTH_CHECK'.
      IF sy-subrc EQ 0.
        CONCATENATE 'NO_AUTH_CHECK = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FILELENGTH'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FILELENGTH'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FILELENGTH = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'DATA_TAB'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'DATA_TAB'.
      IF sy-subrc EQ 0.
        CONCATENATE 'DATA_TAB = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    IF ls_source CS 'FIELDNAMES'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'FIELDNAMES'.
      IF sy-subrc EQ 0.
        CONCATENATE 'FIELDNAMES = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'IV_FIELD_NAME'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'I_LFDNR'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IV_FIELD_NAME = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'EV_VALUE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'E_BUFFR_VALUE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'EV_VALUE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'IM_CODEPAGE' AND ls_source NS 'READ_CODEPAGE'.
      READ TABLE  it_old_par ASSIGNING <l_fs_old_par> WITH KEY par =  'CODEPAGE'.
      IF sy-subrc EQ 0.
        CONCATENATE 'IM_CODEPAGE = ' <l_fs_old_par>-val INTO wa_code1 SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF ls_source CS 'EX_MAPPINGS'.
      wa_code1 = 'EX_MAPPINGS = lv_mappings'.
      LOOP AT it_code INTO lwa_code.
        IF lwa_code CS 'LV_MAPPINGS'.
          lv_tabix = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_tabix IS NOT INITIAL.
        IF lwa_code CS 'DATA:' OR lwa_code CS 'DATA '.
          IF lwa_code CS ','.
            CONCATENATE `DATA: lv_mappings` ' type CPT_SEGTRE,' `"Changes by Haneya Tool` INTO lwa_code.
          ELSE.
            CONCATENATE `DATA: lv_mappings` ' type CPT_SEGTRE.' `"Changes by Haneya Tool` INTO lwa_code.
          ENDIF.
        ELSE.
          IF lwa_code CS ','.
            CONCATENATE ' lv_mappings ' ' type CPT_SEGTRE,' `"Changes by Haneya Tool` INTO lwa_code.
          ELSE.
            CONCATENATE ' lv_mappings ' ' type CPT_SEGTRE.' `"Changes by Haneya Tool` INTO lwa_code.
          ENDIF.
        ENDIF.
        READ TABLE it_code ASSIGNING <lwa_code> INDEX lv_tabix.
        IF <lwa_code> IS ASSIGNED.
          CONCATENATE '*' <lwa_code> INTO <lwa_code>.
        ENDIF.
        UNASSIGN <lwa_code>.
        INSERT lwa_code INTO it_code INDEX lv_tabix.
        CLEAR: lwa_code.
      ELSEIF lv_tabix IS INITIAL.
        LOOP AT it_code INTO lwa_code.
          IF lwa_code CS 'DATA:'.
            EXIT.
          ENDIF.
        ENDLOOP.
        CONCATENATE `DATA:  lv_mappings` ' type CPT_SEGTRE.' `"Changes by Haneya Tool` INTO lwa_code.
        INSERT lwa_code INTO it_code INDEX sy-tabix.
        CLEAR: lwa_code.
      ENDIF.
    ENDIF.
**************************************************************
*    INSERT wa_code1 INTO  it_code  ."INDEX P_TABIX .
    INSERT wa_code1 INTO TABLE it_code  ."INDEX P_TABIX .   " vinod
********************************************************************

    CLEAR wa_code1.
    CLEAR ls_source.
    CLEAR wa_code1.
    p_tabix = p_tabix + 1.
    l_tabix = l_tabix + 1.

  ENDLOOP.
  CLEAR wa_code1.
  p_tabix = p_tabix + 1.
*********************************************************
*  INSERT wa_code1 INTO  it_code ."INDEX P_TABIX  .
  INSERT wa_code1 INTO TABLE it_code ."INDEX P_TABIX  .               " vinod
************************************************************
  p_tabix = p_tabix + 1.

  REFRESH lt_source[].

  PERFORM pretty_printer  CHANGING it_code.
  CLEAR : gv_imp,gv_exp,gv_tab,gv_chg.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM download_file .
  DATA : it_dwnld TYPE STANDARD TABLE OF /vshaneya/hpalm,
         wa_dwnld LIKE LINE OF it_dwnld.
  TYPES: BEGIN OF ty_fnames,
           fnames(30) TYPE c,
         END OF ty_fnames.
  DATA: "gt_excel  TYPE TABLE OF TY_EXCEL,
    lt_fnames   TYPE TABLE OF ty_fnames,
    lv_filename TYPE string,
    p_local     TYPE rlgrap-filename,
    lwa_fnames  TYPE ty_fnames.


  CLEAR:lwa_fnames.
  lwa_fnames = 'Path'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Requirement Name'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Author'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Requirement Type'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Priority'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Direct Cover Status'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Workflow Version'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Product'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Reviewed'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Rich Text'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Target Cycle'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Target Release'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Comments'.
  APPEND lwa_fnames TO lt_fnames.

  CLEAR:lwa_fnames.
  lwa_fnames = 'Description'.
  APPEND lwa_fnames TO lt_fnames.

  DATA : it_rem_lg TYPE STANDARD TABLE OF /vshaneya/rem_lg,
         wa_rem_lg LIKE LINE OF it_rem_lg.
  DATA : lv_count_no TYPE char10,
         lv_req_name TYPE string.

  LOOP AT it_dip INTO wa_dip.
    wa_dwnld-rfcdest = 'Macmillan S/4 HANA upgrade\RTR - Record to Report\RTR060 Define, Maintain and Update Finance Master Data' .
    wa_dwnld-author  = 'visionsoft'.
    wa_dwnld-priority = 'HIGH'.

    CLEAR: lv_count_no, lv_req_name.

    WRITE wa_dip-count_no TO lv_count_no.
    CONDENSE lv_count_no.
    CONCATENATE wa_dip-zprogram lv_count_no INTO lv_req_name.
    wa_dwnld-req_name = lv_req_name.
    wa_dwnld-status = 'Not Completed'.
    wa_dwnld-req_type = 'Functional'.

    DATA lv_desc TYPE string.
    CONCATENATE 'Obsolete :' wa_dip-obsolete ' Replace : ' wa_dip-zreplace
     INTO lv_desc SEPARATED BY space.
    wa_dwnld-description = lv_desc.

    wa_dwnld-workflow_ver = 1 .
    APPEND wa_dwnld TO it_dwnld.
    CLEAR wa_dwnld.
  ENDLOOP.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
    IMPORTING
      file_name     = p_local.

  CLEAR lv_filename.
  lv_filename = p_local.
  CONCATENATE lv_filename '.XLS' INTO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
      write_field_separator   = 'X'
    TABLES
      data_tab                = it_dwnld
      fieldnames              = lt_fnames
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.


FORM sub_pf_status USING rt_extab TYPE slis_t_extab..
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM.                    "sub_pf_status
