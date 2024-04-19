REPORT ZZMMTST2 .

tables: cosp.
perform select_field.
perform where_table.

form select_field.
field-symbols <wkgvalue>.
data: wkgamt like cosp-wkg001.
data: ml_index   like sy-index.

select single * from cosp where wkg001 > 0.
   compute ml_index = 47 + 40.
*  do 10 times.

   assign component ml_index of structure cosp to <wkgvalue>.
   write: /' SELECT_FIELD Routine ', <wkgvalue>.
*  enddo.
*endselect.
endform.

form where_table.

*page 246 of SAP r/3 applications
  data: begin of conditions_tab  occurs 10,
        condition(30)  type c,
        end of conditions_tab.

 write: / 'WHERE_TABLE Routine'.
 move 'WKG001 > 0' to conditions_tab-condition.
 append conditions_tab.
 move ' and WKG002 > 0' to conditions_tab-condition.
 append conditions_tab.

 select * from cosp
    where (conditions_tab).
   write: / 'WKG001=', cosp-wkg001, cosp-wkg002.

 endselect.
endform.



*  write: / 'COSP-WKG001=', cosp-wkg001.
*endselect.





















