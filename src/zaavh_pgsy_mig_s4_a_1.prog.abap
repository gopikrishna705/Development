*&---------------------------------------------------------------------*
*& Report ZAAVH_PGSY_MIG_S4_A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAAVH_PGSY_MIG_S4_A_1.


TABLES : vbak, vbap,
         mara, nsdm_mig_marc. "Added by Heneya Tool
*          MARA, MARC. "Commented by Heneya Tool

DATA gv_vbeln TYPE vbak-vbeln.
DATA lv_valuein  TYPE char10.
DATA lv_valueout TYPE char10.
DATA lv_title    TYPE char10.
DATA lv_answer   TYPE char10.
DATA lt_vbak TYPE STANDARD TABLE OF vbak WITH HEADER LINE.
DATA lt_vbap TYPE STANDARD TABLE OF vbap WITH HEADER LINE.
DATA lt_mara TYPE STANDARD TABLE OF mara WITH HEADER LINE.
DATA ls_vbap TYPE vbap.

CONSTANTS lc_vbeln TYPE char10 VALUE 'S_VBELN'.
CONSTANTS lc_vbak TYPE char10 VALUE 'VBAK'.

CLASS lcl_pgsy_mig_s4 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_data.
    CLASS-METHODS get_pop_up.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS s_vbeln FOR gv_vbeln.
  SELECT-OPTIONS s_vbeln2 FOR gv_vbeln ."Added by Haneya Tool
*SELECT-OPTIONS S_VBELN2 FOR GV_VBELN NO DATABASE SELECTION."Commented by Haneya Tool
  PARAMETERS: p_date TYPE erdat."Added by Haneya Tool
*PARAMETER: P_DATE TYPE ERDAT."Commented by Haneya Tool
  PARAMETERS: p_rb1 RADIOBUTTON GROUP rbg DEFAULT 'X' USER-COMMAND fc1."Added by Haneya Tool
*PARAMETER: P_RB1 RADIOBUTTON GROUP RBG DEFAULT 'X' USER-COMMAND FC1."Commented by Haneya Tool
  PARAMETERS: p_rb2 RADIOBUTTON GROUP rbg."Added by Haneya Tool
*PARAMETER: P_RB2 RADIOBUTTON GROUP RBG."Commented by Haneya Tool
  PARAMETERS: p_rb3 RADIOBUTTON GROUP rbg."Added by Haneya Tool
*PARAMETER: P_RB3 RADIOBUTTON GROUP RBG."Commented by Haneya Tool
  PARAMETERS: p_rb4 RADIOBUTTON GROUP rbg."Added by Haneya Tool
*PARAMETER: P_RB4 RADIOBUTTON GROUP RBG."Commented by Haneya Tool

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN INTO screen.."Added by Haneya Tool
*LOOP AT SCREEN."Commented by Haneya Tool
    IF p_rb1 = 'X'.
      IF screen-name = 'P_RB2'.
        screen-active = 0.
        MODIFY SCREEN FROM screen . "Added by Haneya Tool
*MODIFY SCREEN."Commented by Haneya Tool
      ENDIF.
    ELSE.
      IF screen-name = 'P_RB1'.
        screen-active = 0.
        MODIFY SCREEN FROM screen . "Added by Haneya Tool
*MODIFY SCREEN."Commented by Haneya Tool
      ENDIF.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

*PERFORM GET_DATA(ZAAVH_PGSY_MIG_S4)."Commented by Haneya Tool
  PERFORM get_data IN PROGRAM zaavh_pgsy_mig_s4. "Added by Haneya Tool

FORM get_data.

  LOOP AT lt_vbak.

    IF lt_vbak-vbeln NE space."Added by Haneya Tool
*IF LT_VBAK-VBELN <> SPACE."Commented by Haneya Tool

      LOOP AT lt_vbap WHERE vbeln = lt_vbak-vbeln.

*        REPLACE '+91' INTO lt_vbak-telf1 WITH '+01'.
*        REPLACE 'E'   INTO lt_vbap-route WITH 'H'.

        IF lt_vbap-netpr >= 100.
          lt_vbap-netpr -=  10 ."Added by Haneya Tool
*SUBTRACT 10 FROM LT_VBAP-NETPR."Commented by Haneya Tool
        ELSEIF lt_vbap-netpr LE 100."Added by Haneya Tool
*ELSEIF LT_VBAP-NETPR =< 100."Commented by Haneya Tool
          lt_vbap-netpr *= 2."Added by Haneya Tool
*MULTIPLY LT_VBAP-NETPR BY 2."Commented by Haneya Tool
        ELSE.
          lt_vbap-netpr +=  10 ."Added by Haneya Tool
*ADD 10 TO LT_VBAP-NETPR."Commented by Haneya Tool
        ENDIF.

        lt_vbap-netpr *= 1."Added by Haneya Tool
*DIVIDE LT_VBAP-NETPR BY 1."Commented by Haneya Tool

        IF lt_vbap-matnr NE space.
          LOOP AT lt_mara WHERE matnr EQ lt_vbap-matnr.
            IF lt_mara-matnr NE space."Added by Haneya Tool
*IF LT_MARA-MATNR <> SPACE."Commented by Haneya Tool
**MOVE LT_MARA TO MARA."Commented by Haneya Tool
              mara = lt_mara . "Added by Haneya Tool
              INSERT mara FROM mara . "Added by Haneya Tool
*INSERT MARA."Commented by Haneya Tool
            ENDIF.
          ENDLOOP.

**MOVE LT_VBAP TO VBAP."Commented by Haneya Tool
          vbap = lt_vbap . "Added by Haneya Tool
          UPDATE vbap.
        ENDIF.

      ENDLOOP.
**MOVE LT_VBAK TO VBAK."Commented by Haneya Tool
      vbak = lt_vbak . "Added by Haneya Tool
      MODIFY vbak.
    ENDIF.
    INSERT mara FROM mara . "Added by Haneya Tool
*INSERT MARA."Commented by Haneya Tool
  ENDLOOP.

  CALL TRANSACTION 'VA01' WITH AUTHORITY-CHECK . "Added by Haneya Tool
*CALL TRANSACTION 'VA01'."Commented by Haneya Tool

ENDFORM.

CLASS lcl_pgsy_mig_s4 IMPLEMENTATION.

  METHOD get_data.
    DATA ls_vbak TYPE vbak.
    DATA ls_vbap TYPE vbap.

    CLEAR : lt_vbak[]."Added by Haneya Tool
*REFRESH : LT_VBAK."Commented by Haneya Tool
    SELECT SINGLE * FROM vbak INTO lt_vbak.

    CLEAR : lt_vbap[]."Added by Haneya Tool
*REFRESH : LT_VBAP."Commented by Haneya Tool
    SELECT *
      FROM vbap
      INTO TABLE lt_vbap
      WHERE vbeln EQ lt_vbak-vbeln.

    CLEAR : lt_mara[]."Added by Haneya Tool
*REFRESH : LT_MARA."Commented by Haneya Tool
    SELECT *
      FROM mara
      INTO TABLE lt_mara
      FOR ALL ENTRIES IN lt_vbap
      WHERE matnr EQ lt_vbap-matnr.

  ENDMETHOD.
  METHOD get_pop_up.


  ENDMETHOD.
ENDCLASS.
