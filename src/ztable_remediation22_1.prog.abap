*&---------------------------------------------------------------------*
*& Report ZTABLE_REMEDIATION22
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZTABLE_REMEDIATION22_1.


TYPES : BEGIN OF ty_out,
          matnr TYPE matnr,     " marc
          werks TYPE werks_d,   " marc
          pstat TYPE pstat_d,   " mard
          lfmon TYPE lfmon,     " mard
          sperr TYPE sperr,     " mard
          labst TYPE labst,     " mard
          umlme TYPE umlmd,     " mard
          bwkey TYPE bwkey,     " MBEW
          bwtar TYPE bwtar_d,   " MBEW
          lgort TYPE lgort_d,   " MCHB
          charg TYPE charg_d,   " MCHB
          lvorm TYPE lvocb,     " MCHB
          ersda TYPE ersda,     " MCHB
          ernam TYPE ernam,     " MCHB
          kunnr TYPE kunnr,     " MCSD
          sdspr TYPE sperr,     " MCSD
          sdlab TYPE labst,     " MCSD
          mblnr TYPE mblnr,     " MKPF
          mjahr TYPE mjahr,     " MKPF
          vgart TYPE vgart,     " MKPF
          blart TYPE blart,     " MKPF
        END OF ty_out.

DATA : it_out TYPE STANDARD TABLE OF ty_out,
       is_out TYPE ty_out.
DATA : l_matnr TYPE mara-matnr.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : v_matnr FOR l_matnr.
SELECTION-SCREEN END OF BLOCK b1.

IF v_matnr IS NOT INITIAL.
  SELECT matnr,
         pstat FROM mara
               INTO TABLE @DATA(it_mara)
               WHERE matnr IN @v_matnr.
  IF it_mara IS NOT INITIAL.
    SELECT matnr,
           werks FROM marc
                 INTO TABLE @DATA(it_marc)
                 FOR ALL ENTRIES IN @it_mara
                 WHERE matnr EQ @it_mara-matnr.
    SELECT matnr,
           kunnr,
           sdspr,
           sdlab FROM mcsd
                 INTO TABLE @DATA(it_mcsd)
                 FOR ALL ENTRIES IN @it_mara
                 WHERE matnr EQ @it_mara-matnr.

    IF it_marc IS NOT INITIAL.
      SELECT matnr,
             pstat,
             lfmon,
             sperr,
             labst,
             lfgja,
             umlme FROM mard
                   INTO TABLE @DATA(it_mard)
                   FOR ALL ENTRIES IN @it_marc
                   WHERE matnr EQ @it_marc-matnr.
      IF it_mard IS NOT INITIAL.
        SELECT matnr,
               bwkey,
               bwtar FROM mbew
                     INTO TABLE @DATA(it_mbew)
                     FOR ALL ENTRIES IN @it_mard
                     WHERE matnr EQ @it_mard-matnr.
        SELECT matnr,
               lgort,
               charg,
               lvorm,
               ersda,
               ernam FROM mchb
                     INTO TABLE @DATA(it_mchb)
                     FOR ALL ENTRIES IN @it_mard
                     WHERE matnr EQ @it_mard-matnr.

        SELECT mblnr,
               mjahr,
               vgart,
               blart FROM mkpf
                     INTO TABLE @DATA(it_mkpf)
                     FOR ALL ENTRIES IN @it_mard
                     WHERE mjahr EQ @it_mard-lfgja.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
