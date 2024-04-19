FUNCTION Z_GET_PROJ_FINAL_STATUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IOBJNR) LIKE  PRPS-OBJNR
*"  EXPORTING
*"     VALUE(OSTATUS) LIKE  JCDS-STAT
*"----------------------------------------------------------------------

TABLES: JCDS.

DATA:  MAX_DATE         LIKE JCDS-UDATE,
       MAX_TIME         LIKE JCDS-UTIME.

   SELECT MAX( UDATE ) FROM JCDS INTO MAX_DATE    "Select Latest Date
          WHERE OBJNR = IOBJNR
            AND INACT <> 'X'.                      "Not Inactie
       IF SY-SUBRC <> 0.
          MOVE 'E9999' TO OSTATUS.
       ENDIF.
   SELECT MAX( UTIME ) FROM JCDS INTO MAX_TIME     "Select Latest Time
          WHERE OBJNR = IOBJNR
            AND UDATE = MAX_DATE
            AND INACT <> 'X'.                      "Not Inactie
       IF SY-SUBRC <> 0.
            MOVE 'E9999' TO OSTATUS.
       ENDIF.

       SELECT SINGLE * FROM JCDS      "Select Status
           WHERE OBJNR = IOBJNR
*             AND STAT = INSTAT        "Status
             AND UDATE = MAX_DATE     "Creation date of change document
             AND UTIME = MAX_TIME     "Creation time of change document
             AND INACT <> 'X'.        "Not Inactie

       IF SY-SUBRC = 0.
          OSTATUS = JCDS-STAT.         "Status
       ELSE.
          OSTATUS = 'E9999'.
       ENDIF.

ENDFUNCTION.
