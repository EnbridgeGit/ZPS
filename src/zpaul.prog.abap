REPORT ZPAUL NO STANDARD PAGE HEADING LINE-COUNT 65.
TABLES: BKPF, BSEG.
DATA  : Z_COUNT TYPE I.
DATA  : Z_BUKRS1 LIKE BKPF-BUKRS.
DATA  : Z_BUKRS2 LIKE BKPF-BUKRS.
DATA  : Z_BELNR  LIKE BKPF-BELNR.
DATA  : Z_GJAHR  LIKE BKPF-GJAHR.
DATA  : BUKRS_PREV LIKE BKPF-BUKRS.
DATA  : COUNT TYPE I.
DATA  : BEGIN OF IBSEG,
        BUZEI LIKE BSEG-BUZEI,
        BUKRS LIKE BSEG-BUKRS,
        END OF IBSEG.
*******************************  MAIN  *********************************
START-OF-SELECTION.
WRITE: /.
WRITE: / 'DOCUMENT COUNT BY COMPANY  CODE'.
WRITE: / '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'.
WRITE: /.
WRITE: / 'Query #1 Fiscal Year 1998/Fiscal Period 04'.
WRITE: /.
WRITE: /'Company Documents'.
SELECT COUNT( DISTINCT BELNR ) BUKRS INTO (Z_COUNT, Z_BUKRS1)
                                FROM BKPF
                                WHERE GJAHR = '1998' AND
                                MONAT = '04'
                                GROUP BY BUKRS.
WRITE: / Z_BUKRS1, Z_COUNT.
ENDSELECT.
WRITE: /.
WRITE: / 'Query #2 Fiscal Year 1998/Fiscal Period 06'.
WRITE: /.
WRITE: /'Company Documents'.
SELECT COUNT( DISTINCT BELNR ) BUKRS INTO (Z_COUNT, Z_BUKRS1)
                                FROM BKPF
                                WHERE GJAHR = '1998' AND
                                MONAT = '06'
                                GROUP BY BUKRS.
WRITE: / Z_BUKRS1, Z_COUNT.
ENDSELECT.
COUNT = 0.
************************************************************************
* COMPANY   BUKRS
* DOCUMENT  BELNR
* LINE ITEM BUZEI
*
* FIND THE TOTAL NUMBER OF LINE ITEMS BY COMPANY
* THERE ARE SEVERAL LINE ITEMS PER DOCUMENT
* THERE ARE SEVERAL DOCUMENTS PER COMPANY
************************************************************************
WRITE: /.
WRITE: / 'LINE ITEM COUNT BY COMPANY CODE'.
WRITE: / '+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+'.
WRITE: /.
WRITE: / 'Query #1 Fiscal Year 1998/Fiscal Period 04'.
WRITE: /.
WRITE: /'Company Documents'.
COUNT = 0.
BUKRS_PREV = 'XXXX'.
SELECT DISTINCT BUKRS GJAHR BELNR INTO
                              (Z_BUKRS1, Z_GJAHR, Z_BELNR) FROM BKPF
      WHERE GJAHR = '1998' AND
      MONAT = '04'.

      IF BUKRS_PREV <> Z_BUKRS1.
         IF BUKRS_PREV <> 'XXXX'.
            WRITE :/ BUKRS_PREV, COUNT.
            COUNT = 0.
         ENDIF.
         BUKRS_PREV = Z_BUKRS1.
      ENDIF.

      SELECT BUZEI BUKRS INTO IBSEG
             FROM BSEG
             WHERE
                 BUKRS = Z_BUKRS1 AND
                 BELNR = Z_BELNR AND
                 GJAHR = Z_GJAHR.
             COUNT = COUNT + 1.
       ENDSELECT.
ENDSELECT.
WRITE: /.
WRITE: / 'Query #2 Fiscal Year 1998/Fiscal Period 06'.
WRITE: /.
WRITE: /'Company Documents'.
COUNT = 0.
BUKRS_PREV = 'XXXX'.
SELECT DISTINCT BUKRS GJAHR BELNR INTO
                              (Z_BUKRS1, Z_GJAHR, Z_BELNR) FROM BKPF
      WHERE GJAHR = '1998' AND
      MONAT = '06'.

      IF BUKRS_PREV <> Z_BUKRS1.
         IF BUKRS_PREV <> 'XXXX'.
            WRITE :/ BUKRS_PREV, COUNT.
            COUNT = 0.
         ENDIF.
         BUKRS_PREV = Z_BUKRS1.
      ENDIF.

      SELECT BUZEI BUKRS INTO IBSEG
             FROM BSEG
             WHERE
                 BUKRS = Z_BUKRS1 AND
                 BELNR = Z_BELNR AND
                 GJAHR = Z_GJAHR.
             COUNT = COUNT + 1.
       ENDSELECT.
ENDSELECT.
