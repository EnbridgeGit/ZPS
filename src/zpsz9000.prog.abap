REPORT zpsz9000 LINE-COUNT 58 LINE-SIZE 170.

************************************************************************
*  Author:      Cathy McCoy
*  Date:        July 2001
*  Description:
*      The purpose of this program it to update table z9000 with  * * *
*  Company Code data to correspond with Main Asset Number.
************************************************************************

TABLES: z9000,          "Match Asset w Comp Code
        anla,          "Main Asset Number
        t001.          "Company Code


DATA: BEGIN OF int_z9000 OCCURS 0,
      mandt like z9000-mandt,
      wbsel LIKE z9000-wbsel,
      bukrs LIKE z9000-bukrs,
      anln1 LIKE z9000-anln1,
END OF int_z9000.

*add company code for asset #
START-OF-SELECTION.

  SELECT * FROM z9000 INTO CORRESPONDING FIELDS OF int_z9000
      WHERE  bukrs = space.
  SELECT * FROM anla where
      anln1 = int_z9000-anln1.
      MOVE anla-bukrs TO int_z9000-bukrs.
      append int_z9000.
      insert into z9000 values int_z9000.
      clear int_z9000.

  ENDSELECT.
endselect.

*
*delete empty records
  LOOP AT z9000.
    DELETE FROM Z9000 WHERE bukrs = space
    AND anln1 >< 'XXXXXXXXXXXX'.
  ENDLOOP.
*
**report changes
  WRITE: / 'The Table Z9000 has been updated'.


