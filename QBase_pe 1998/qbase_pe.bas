30 REMark QL dbas front end package program
60 REMark $$asmb=win1_basic_qbase_exp_conf,0,12
90 REMark $$asmb=win1_basic_qbase_cde,4,82
120 REMark $$stak=16384
150 REMark WHEN ERRor
180 REMark ERROR_HANDLING
210 REMark END WHEN
240 REMark IF SCR_XLIM=512 THEN a=ITEM_SELECT('Sorry',"This program doesn't work at this resolution",'OK',,,0,0):STOP
270 JOB_NAME "Database Editor"
300 DIM choice$(3,26)
330 DIM choice2$(15,15)
360 m$=C_STRG$(1)
390 p$=C_STRG$(2)
420 e$=C_STRG$(3)
450 r$=C_SEL$(1)
480 minwid=m$
510 prdev$=p$
540 ext_ed$=e$
570 p="1" INSTR r$
600 SELect ON p
630   =1
660     scr_x=512:scr_y=256
690   =2
720     scr_x=640:scr_y=480
750   =3
780     scr_x=800:scr_y=600
810   =4
840     scr_x=1024:scr_y=768
870 END SELect
900 :
930 REMark set up main menu
960 :
990 dbname$="":status$=""
1020 search_string$=""
1050 changed%=1
1080 tabb=0
1110 :
1140 REMark setup secondary menu
1170 :
1200 :
1230 REMark open console window and size it to max size of screen
1260 :
1290 OPEN#6,'con_' : REMark menu window channel
1320 OPEN #3,'con_' : REMark display application window
1350 OPEN #7,'con_' : REMark status bar
1380 OPEN #8,'con_'
1410 REMark OUTLN
1440 :
1470 REMark Draw the menu screen, make all loose items except the file menu button unavailable
1500 :
1530 MDRAW#6,'qbase1',0,0
1560 screen_size=1
1590 scr_x=562:scr_y=424 : REMark temporarily sey screen size variables manually.
1620 FOR button=-2 TO -13 STEP -1: a=MSTAT%(#6,button,-1): NEXT button
1650 a=MSTAT%(#6,-17,-1)
1680 a=MSTAT%(#6,-18,-1)
1710 REMark MAKE_SCREEN
1740 REPeat main_loop
1770   action=MCALL(#6;\0)
1800   REMark PRINT #0,action, MAWNUM(#6,action,coll%,roww%):PRINT#0, coll%,roww%:PRINT#0, scr_contents$(coll%,roww%)
1830   IF action > 65536 THEN EDIT_RECORD
1860   IF action = -19 THEN
1890       GET_POINTER_POS
1920          IF result%(5)=1 THEN SMALLER
1950          IF result%(6)=2 THEN BIGGER
1980       a=MSTAT%(#6,-19,0)
2010   END IF
2040   SELect ON action
2070     =-0: er=FDB_CLOSE_DATA(#4):CLS#3:MCLEAR #6:CLOSE #6:CLOSE #7:CLAMP:EXIT main_loop
2100     =-1: FILE_MENU:er=FDB_RPOSAB(#4,0):MDRAW#6: a=MSTAT%(#6,-1,0):IF NOT er THEN MAKE_SCREEN: PRINT_ALL_FIELDS:REMark open database
2130     =-3: RPOSRE#4;1:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-3,0):                REMark next record
2160     =-4: RPOSRE#4;-1:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-4,0):               REMark previous record
2190     =-5: RPOSAB#4,0:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-5,0):                REMark go to first record
2220     =-6: RPOSAB#4,COUNT(#4):UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-6,0):        REMark go to last record
2250     =-7: ADD_REC:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-7,0):                   REMark add record
2280     =-8: DELETE_RECORD:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-8,0):             REMark delete record
2310       REMark =-9: add=0:EDIT_REC:UPDATE_STATUS:PRINT_ALL_FIELDS: a=MSTAT%(#6,-9,0):              REMark update record
2340     =-12:curr_rec=RECNUM(#4):FIND_STRING:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-12,0): REMark find record
2370     =-13: curr_rec=RECNUM(#4):FIND_AGAIN:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-13,0): REMark find again
2400     =-17: ORDER_DATA_BASE:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS: a=MSTAT%(#6,-17,0):                REMark SORT
2430     =-18: INCLUDE_FIELDS:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-18,0):              REMark INCLUDE
2460     =-2: tabb=1:temprecnum=RECNUM(#4):tabular:RPOSAB#4,temprecnum:tabb=0:UPDATE_STATUS:MAKE_SCREEN:PRINT_ALL_FIELDS
2490     =-9: NEW_FIELD:UPDATE_STATUS:RECALCULATE_POSITIONS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-9,0):     REMark add field
2520     =-10: DELETE_FIELD:UPDATE_STATUS:RECALCULATE_POSITIONS:MAKE_SCREEN:PRINT_ALL_FIELDS:a=MSTAT%(#6,-10,0):REMark delete field
2550     =-11: RENAME_FIELD:UPDATE_STATUS:RECALCULATE_POSITIONS:MAKE_SCREEN:PRINT_ALL_FIELDS: a=MSTAT%(#6,-11,0):REMark rename field
2580     REMark =-19:bigger:a=MSTAT%(#6,-19,0)
2610     REMark =-20:SMALLER:a=MSTAT%(#6,-20,0)
2640       REMark    =15: CLOSE_DATA#4:CLS#3:EXIT secondary_loop
2670     =REMAINDER :
2700   END SELect
2730 END REPeat main_loop
2760 :
2790 DEFine PROCedure GET_POINTER_POS
2820    DIM result%(15)
2850    PVAL #6,result%
2880    px%=result%(14):py%=result%(15)
2910    winx%=result%(8):winy%=result%(9)
2940 END DEFine GET_POINTER_POS
2970 :
3000 DEFine PROCedure SMALLER
3030   LOCal screen$,x,y
3060   screen$=""
3090   GET_POINTER_POS
3120   IF screen_size=1 THEN RETurn
3150   screen_size=screen_size-1
3180   screen$="qbase"&screen_size
3210   IF tabb=1 THEN a=MAWNUM(#6\1,x%,y%)
3240   MCLEAR#6
3270   MDRAW#6,screen$,0,0
3300   SET_SCR
3330   er=FDB_RPOSRE(#4;0) :REMark test channel is open
3360   IF NOT er THEN
3390      IF tabb=1 THEN
3420          changed%=0: UPDATE_TABLE x%,y%
3450      ELSE
3480         MAKE_SCREEN:PRINT_ALL_FIELDS
3510      END IF
3540   END IF
3570 RDPT#6,48,px%,py%
3600 END DEFine SMALLER
3630 :
3660 DEFine PROCedure BIGGER
3690   LOCal screen$,x%,y%
3720   screen$=""
3750   GET_POINTER_POS
3780   IF screen_size=7 THEN RETurn
3810   REMark IF SCR_XLIM=640 THEN RETurn
3840   REMark IF SCR_XLIM=800 AND screen_size=3 THEN RETurn
3870   IF winx%+50>SCR_XLIM-10 THEN RETurn
3900   IF winy%+50>SCR_YLIM-10 THEN RETurn
3930   screen_size=screen_size+1
3960   screen$="qbase"&screen_size
3990   IF tabb=1 THEN a=MAWNUM(#6\1,x%,y%)
4020   MCLEAR#6
4050   MDRAW#6,screen$,0,0
4080   SET_SCR
4110   er=FDB_RPOSRE(#4;0) :REMark test channel is open
4140   IF NOT er THEN
4170      IF tabb =1 THEN
4200          changed%=0: UPDATE_TABLE x%,y%
4230      ELSE
4260         MAKE_SCREEN:PRINT_ALL_FIELDS
4290      END IF
4320   END IF
4350 RDPT#6,48,px%,py%
4380 END DEFine BIGGER
4410 :
4440 DEFine PROCedure SET_SCR
4470   SELect ON screen_size
4500     =1 : scr_x=562:scr_y=424
4530     =2 : scr_x=612:scr_y=474
4560     =3 : scr_x=662:scr_y=524
4590     =4 : scr_x=712:scr_y=574
4620     =5 : scr_x=762:scr_y=624
4650     =6 : scr_x=812:scr_y=674
4680     =7 : scr_x=862:scr_y=721
4710 END SELect
4740 END DEFine SET_SCR
4770 :
4800 DEFine PROCedure FILE_MENU
4830   MDRAW#8,'file'
4860   REPeat act_loop
4890     action=MCALL(#8;\0)
4920     SELect ON action
4950       =-1: OPEN_DB:EXIT act_loop
4980       =-2: CREATE_DB:a=MSTAT%(#8,-2,0)
5010       =-3: ABOUT_MENU:a=MSTAT%(#8,-3,0)
5020       =-5: EXPORT_FILE:a=MSTAT%(#8,-5,0)
5040       =-6: IMPORT_FILE: a=MSTAT%(#8,-6,0)
5070       =0: MCLEAR#8:RETurn
5100     END SELect
5130   END REPeat act_loop
5160 MCLEAR #8
5190 END DEFine FILE_MENU
5220 :
5250 DEFine PROCedure IMPORT_FILE
5280  REPeat until_correct
5310    ifile$=FILE_SELECT$("Import file name",DATAD$&"data_exp",-1,_exp)
5340    dbname$=FILE_SELECT$("New Database",DATAD$&"new_data_base_dbs",-1,_dbs)
5370    er=FDB_IMPORT(ifile$ TO #10;dbname$)
5400    IF er=0 THEN EXIT until_correct
5430    a=FILE_ERROR (er)
5460    IF a=0 THEN m=MSTAT%(#8,-6,0):RETurn
5490  END REPeat until_correct
5520 CLOSE_DATA #10
5550 END DEFine IMPORT_FILE
5580 :
5610 DEFine PROCedure EXPORT_FILE
5640    efile$=FILE_SELECT$("Export file name",DATAD$&"data_exp",-1,_exp)
5670    REPeat until_correct
5700    er=FDB_EXPORT(#4 TO efile$)
5730    IF er=0 THEN EXIT until_correct
5740    a=FILE_ERROR(er)
5750    IF a=0 THEN m=MSTAT%(#8,-5,0): RETurn
5755  END REPeat until_correct
5757 END DEFine EXPORT_FILE
5760 :
5790 DEFine PROCedure ABOUT_MENU
5820   OPEN#9,'con_'
5850     MDRAW#9,'about'
5880     REPeat about_loop
5910       action=MCALL(#9,\0)
5940       IF action=0 THEN MCLEAR#9:RETurn
5970     END REPeat about_loop
6000 END DEFine ABOUT_MENU
6030 :
6060 DEFine PROCedure OPEN_DB
6090   REMark LOCal dbname$,no_fields,f_type(10),f_len(10),f_name$(10,10)
6120   dbname$=""
6150   REPeat until_correct
6180   dbname$=FILE_SELECT$("Open Database",DATAD$&"new_data_base_dbs",-1,_dbs)
6210   REMark test for file errors
6240   IF dbname$="" THEN RETurn
6270   er=FDB_OPEN_DATA(#4;dbname$)
6300   IF er=0 THEN EXIT until_correct
6330   IF er=-7 THEN
6360      a=FILE_ERROR(-7)
6390      IF a=0 THEN
6420         CLOSE#4:MCLEAR#6:CLOSE#6:CLOSE #7:CLAMP:STOP
6450      END IF
6480   END IF
6510   dbname$=""
6540   END REPeat until_correct
6570   :
6600   REMark get field names, types, lengths  etc and put them in an array
6630   :
6660   UPDATE_STATUS
6690   no_fields=FLNUM(#4)
6720   DIM f_type(no_fields-1):DIM f_len(no_fields-1):DIM f_name$(no_fields-1,20)
6750   DIM flxpos(no_fields-1),flypos(no_fields-1)
6780   FOR i=0 TO no_fields-1
6810     f_name$(i)=FLNAME(#4;i+1):f_type(i)=FLTYP(#4;i+1):f_len(i)=FLLEN(#4;i+1)
6840   END FOR i
6870   :
6900   REMark calculate screen positions; simple as of now
6930   :
6960   p=2
6990   FOR i=0 TO no_fields-1
7020     flxpos(i)=p
7050     flypos(i)=2
7080   IF FLNUM(#4)>INT(scr_y/10/2-2) THEN p=p+1: incr=1:ELSE p=p+2 :incr=2:END IF
7110   no_lines=f_len(i)/((scr_x/6)-2)
7140   IF incr<INT(no_lines) THEN p=p+INT(no_lines-l)
7170 END FOR i
7200 FOR i=-1 TO -13 STEP -1: a=MSTAT%(#6, i, 0): NEXT i
7230 a=MSTAT%(#6, -17, 0)
7260 a=MSTAT%(#6, -18, 0)
7290 END DEFine OPEN_DB
7320 :
7350 REMark MAKE_SCREEN
7380 REMark PRINT_ALL_FIELDS
7410 :
7440 DEFine PROCedure RECALCULATE_POSITIONS
7470   no_fields=FLNUM(#4)
7500   DIM f_type(no_fields-1):DIM f_len(no_fields-1):DIM f_name$(no_fields-1,20)
7530   DIM flxpos(no_fields-1),flypos(no_fields-1)
7560   FOR i=0 TO no_fields-1
7590     f_name$(i)=FLNAME(#4;i+1):f_type(i)=FLTYP(#4;i+1):f_len(i)=FLLEN(#4;i+1)
7620   END FOR i
7650   :
7680   REMark calculate screen positions; simple as of now
7710   :
7740   p=2
7770   FOR i=0 TO no_fields-1
7800     flxpos(i)=p
7830     flypos(i)=2
7860   IF FLNUM(#4)>INT(scr_y/10/2-2) THEN p=p+1: incr=1:ELSE p=p+2 :incr=2:END IF
7890   no_lines=f_len(i)/((scr_x/6)-2)
7920   IF incr<INT(no_lines) THEN p=p+INT(no_lines-l)
7950 END FOR i
7980 END DEFine RECALCULATE_POSITIONS
8010 :
8040 DEFine PROCedure MAKE_SCREEN
8070   LOCal xpos,ypos
8100   DIM scr_contents$(1,no_fields-1,50)
8130   DIM x_width%(1)
8160   :
8190   REMark make sure screen title is always centered
8220   :
8250   tit$=" Q-Base v0.1P : "&dbname$&status$
8280   MWLINK#6,1!#7
8310   INK#7,0:PAPER#7,221
8340   PRINT #7,tit$
8370   MWLINK #6,1,#3
8400   CLS#3
8430   INK #3,7
8460   x_width%(0)=scr_x/4
8490   x_width%(1)=x_width%(0)*3-15
8520   FOR i=0 TO no_fields-1
8550     scr_contents$(0,i)=f_name$(i)
8580     scr_contents$(1,i)=FETCH(#4,i+1)
8610   END FOR i
8640 END DEFine MAKE_SCREEN
8670 :
8700 DEFine PROCedure PRINT_ALL_FIELDS
8730   LOCal i
8760   MAWDRAW #6,1,scr_contents$,0,0,,x_width%
8790   FOR i=1 TO FLNUM(#4)*2 STEP 2
8820     stat%=MSTAT%(#6,i*65536+1,-1)
8850     REMark PRINT #0,i;
8880   END FOR i
8910 END DEFine PRINT_ALL_FIELDS
8940 :
8970 DEFine PROCedure PRINT_FIELD(x,y,field_name$)
9000   AT#3,x,y
9030   PRINT #3,field_name$
9060 END DEFine PRINT_FIELD
9090 :
9120 DEFine PROCedure EDIT_RECORD
9150   LOCal num,f,f%,col%,row%,edittext$,text$,i,f$
9180   GET_POINTER_POS
9210   MWINDOW#6,action
9240   text$=MTEXT$(#6,action)
9270      IF result%(5)=1 THEN HOT_STUFF text$: a=MSTAT%(#6,action,0):RETurn : REMark stuff stuffer buffer on left mouse click
9300   MINPUT#6,text$
9330   key%=MKEY%(#6)
9360   num=MAWNUM(#6,action,col%,row%)
9390   IF key%=10 THEN
9420     i=row%+1
9450     f$=text$
9480     typ=f_type(row%)
9510     SELect ON typ
9540       =0:er=FDB_SET(#4;i;f$):
9570       =1:IF NUMBER(f$) AND BOUNDS(f$) THEN f%=f$:er=FDB_SET(#4;i;f%)
9600       =2:IF NUMBER(f$) THEN f=f$:er=FDB_SET(#4;i;f)
9630       =3:IF NUMBER(f$) THEN f=f$:er=FDB_SET(#4;i;f)
9660     END SELect
9690     IF NOT NUMBER(f$) AND typ<>0 THEN REPORT_ERROR -17: MAKE_SCREEN: PRINT_ALL_FIELDS: RETurn
9720     IF NOT BOUNDS(f$) AND typ=1 THEN REPORT_ERROR -18: MAKE_SCREEN: PRINT_ALL_FIELDS: RETurn
9750     IF er=-5 THEN REPORT_ERROR -5:MAKE_SCREEN:PRINT_ALL_FIELDS: RETurn
9780     IF er=-17 THEN REPORT_ERROR -17:MAKE_SCREEN:PRINT_ALL_FIELDS: RETurn
9810     UPDATE#4
9840     MAKE_SCREEN:PRINT_ALL_FIELDS
9870   END IF
9900 END DEFine EDIT_RECORD
9930 :
9960 DEFine PROCedure ADD_REC
9990   LOCal f$,typ,i,c
10020   c=COUNT(#4)+1:status$=" : Record "&c&" of "&c:MAKE_SCREEN
10050   FOR i=0 TO no_fields-1
10080     typ=f_type(i)
10110     SELect ON typ
10140       =0:SET#4;i+1;""
10170       =1:SET#4;i+1;0
10200       =2:SET#4;i+1;0
10230       =3:SET#4;i+1;0
10260     END SELect
10290   END FOR i
10320   APPEND#4
10350   RPOSAB#4,COUNT(#4)
10380 END DEFine ADD_REC
10410 :
10440 DEFine PROCedure WHOLE_SCREEN
10470   MAKE_SCREEN
10500   PRINT_ALL_FIELDS
10530 END DEFine WHOLE_SCREEN
10560 :
10590 DEFine PROCedure NEW_FIELD
10620   LOCal field_choice,type,name$,length,length$
10650   name$=""
10680   length$=""
10710   length=-20: REMark default length
10740   type = 0: REMark default type
10770   field_choice=FLNUM(#4)+1: REMark default to append
10800   MDRAW#8,'addfield'
10830   MAWDRAW#8,1,f_name$,0,0,,105
10860   REPeat action_loop
10890     action=MCALL(#8; \0)
10920     IF action > 65536 THEN
10950       a=MSTAT%(#8,-9,0)
10980       tempaction=action
11010       FOR i=1 TO FLNUM(#4)
11040         a=MSTAT%(#8,i*65536+1,0)
11070       END FOR i
11100       a=MSTAT%(#8,tempaction,1)
11130       field_choice=MAWNUM(#8,action)
11160     END IF
11190     SELect ON action
11220       =0: MCLEAR#8:RETurn
11250       =-1:a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-3,0):a=MSTAT%(#8,-4,0):type=0
11280       =-2:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-3,0):a=MSTAT%(#8,-4,0):type=1
11310       =-3:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-4,0):type=2
11340       =-4:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-3,0):type=3
11370       =-10:MWINDOW#8,-10:PAPER #8,7:MINPUT#8,length$:length=length$
11400       =-8:MWINDOW#8,-8:PAPER #8,7:MINPUT#8,name$
11430       =-5: MCLEAR #8: EXIT action_loop
11460     =-9: field_choice=FLNUM(#4)+1:FOR i=1 TO FLNUM(#4):a=MSTAT%(#8,i*65536+1,0):END FOR i
11490   END SELect
11520 END REPeat action_loop
11550 IF name$="" THEN RETurn
11580 IF type = 0 AND name$(LEN(name$))<>"$" THEN
11610   name$=name$&"$"
11640 END IF
11670 message$ = "Add a field called "&name$&" of type "&type&"?"
11700 final_choice=ITEM_SELECT("Confirm",message$,"Yes","No")
11730 IF final_choice<>1 THEN RETurn
11760 IF final_choice=1 AND type=0 THEN
11790   ADD_FIELD#4;field_choice,type,length
11820 END IF
11850 IF final_choice=1 AND type <> 0 THEN
11880   ADD_FIELD#4;field_choice,type,0
11910 END IF
11940 STNAME#4;field_choice,name$
11970 SAVE_NAMES#4
12000 END DEFine NEW_FIELD
12030 :
12060 DEFine PROCedure DELETE_FIELD
12090   LOCal field_choice,a,tempaction,final_choice
12120   IF FLNUM(#4)=1 THEN a=ITEM_SELECT ("Warning","You can't delete the last field in a file","OK"):RETurn
12150   MDRAW#8,'delfield'
12180   MAWDRAW#8,1,f_name$,0,0,,105
12210   REPeat action_loop
12240     action=MCALL(#8; \0)
12270     IF action > 65536 THEN
12300       tempaction=action
12330       FOR i=1 TO FLNUM(#4)
12360         a=MSTAT%(#8,i*65536+1,0)
12390       END FOR i
12420       a=MSTAT%(#8,tempaction,1)
12450       field_choice=MAWNUM(#8,action)
12480     END IF
12510     IF field_choice=0 THEN MCLEAR#8:RETurn
12540     SELect ON action
12570       =0: MCLEAR#8:RETurn
12600       =-1: MCLEAR #8: EXIT action_loop
12630     END SELect
12660   END REPeat action_loop
12690   final_choice=ITEM_SELECT("Confirm","Are you sure you want to delete the field "&f_name$(field_choice-1),"Yes","No")
12720   IF final_choice<>1 THEN RETurn
12750   REMOVE_FIELD #4,field_choice
12780 END DEFine DELETE_FIELD
12810 :
12840 DEFine PROCedure RENAME_FIELD
12870   LOCal field_choice, new_name$
12900   new_name$=""
12930   MDRAW#8,'renfield'
12960   MAWDRAW#8,1,f_name$,0,0,,105
12990   REPeat action_loop
13020     action=MCALL(#8; \0)
13050     IF action > 65536 THEN
13080       tempaction=action
13110       FOR i=1 TO FLNUM(#4)
13140         a=MSTAT%(#8,i*65536+1,0)
13170       END FOR i
13200       a=MSTAT%(#8,tempaction,1)
13230       field_choice=MAWNUM(#8,action)
13260     END IF
13290     SELect ON action
13320       =0:  MCLEAR#8:RETurn
13350       =-1: MCLEAR #8: EXIT action_loop
13380       =-4: MWINDOW#8,-4:PAPER#8,7:MINPUT#8,new_name$
13410     END SELect
13440   END REPeat action_loop
13470   IF FLTYP(#4,field_choice)=0 AND new_name$(LEN(new_name$))<>"$" THEN
13500     new_name$=new_name$&"$"
13530   END IF
13560   IF new_name$="" THEN RETurn
13590   final_choice=ITEM_SELECT("Confirm","Are you sure you want to rename the field "&f_name$(field_choice-1)&" to "&new_name$,"Yes","No")
13620   IF final_choice<>1 THEN RETurn
13650   STNAME#4;field_choice,new_name$
13680   SAVE_NAMES #4
13710 END DEFine RENAME_FIELD
13740 :
13770 DEFine PROCedure CREATE_DB
13800     er=FDB_CLOSE_DATA(#4)
13830     dbname$=FILE_SELECT$("New Database",DATAD$&"new_data_base_dbs",-1,_dbs)
13860     REPeat until_correct
13890       er=FDB_CREATE(#4;dbname$; 0,-20; 0,-20)
13920       IF er=0 THEN EXIT until_correct
13950       IF er=-8 THEN REPORT_ERROR -8: RETurn
13980       IF er=-7 THEN REPORT_ERROR -7: RETurn
14010     END REPeat until_correct
14040     SEXTRA #4;'"field1$","field2$"'&CHR$(13)&CHR$(10)
14070     APPEND#4
14100     CLOSE_DATA#4
14130 END DEFine CREATE_DB
14160 :
14190 DEFine PROCedure CHANGE_DATA_DIR
14220   ddir$=DIR_SELECT$("Select new data directory",,,,,,"win3_dbas_")
14250   DATA_USE ddir$
14280 END DEFine CHANGE_DATA_DIR
14310 :
14340 DEFine PROCedure UPDATE_STATUS
14370   r=RECNUM(#4)+1
14400   c=COUNT(#4)
14430   status$=" : Record "&r&" of "&c&"                "
14460 END DEFine UPDATE_STATUS
14490 :
14520 DEFine FuNction PIX_CENT(wid,STRING$)
14550   LOCal w$
14580   w$=wid/2-((LEN(STRING$)*6)/2)
14610   RETurn w$
14640 END DEFine PIX_CENT
14670 :
14700 DEFine PROCedure FIND_STRING
14730   search_string$=READ_STRING$("Input a string",,"String to find:")
14760   FIND#4;search_string$
14790   IF FOUND(#4)=0
14820     a=ITEM_SELECT("Error",search_string$&" not found",CHR$(27))
14850     RPOSAB#4;curr_rec
14880   END IF
14910 END DEFine FIND_STRING
14940 :
14970 DEFine PROCedure FIND_AGAIN
15000   IF search_string$="" THEN RETurn
15030   FINDC#4;search_string$
15060   IF FOUND(#4)=0
15090     a=ITEM_SELECT("Error",search_string$&" not found",CHR$(27))
15120     RPOSAB#4;curr_rec
15150   END IF
15180 END DEFine FIND_AGAIN
15210 :
15240 DEFine PROCedure ORDER_DATA_BASE
15270   LOCal action,title$,lin,column,direction,direc$,field
15300   REMark set defaults
15330   direction=1:field_choice=1
15360   MDRAW#8,'sort'
15390   MAWDRAW#8,1,f_name$,0,0,,105
15420   REPeat action_loop
15450     action=MCALL(#8;\0)
15480     IF action > 65536 THEN
15510       tempaction=action
15540       FOR i=1 TO FLNUM(#4)
15570         a=MSTAT%(#8,i*65536+1,0)
15600       END FOR i
15630       a=MSTAT%(#8,tempaction,1)
15660       field_choice=MAWNUM(#8,action)
15690     END IF
15720     SELect ON action
15750       =0: MCLEAR#8: RETurn
15780       =-1:a=MSTAT%(#8,-5,0):direction=1
15810       =-5:a=MSTAT%(#8,-1,0):direction=-1
15840       =-2:MCLEAR#8:EXIT action_loop
15870     END SELect
15900   END REPeat action_loop
15930   ORDER#4;field_choice,direction
15960 END DEFine ORDER_DATA_BASE
15990 :
16020 DEFine PROCedure INCLUDE_FIELDS
16050   LOCal criterion$
16080   REMark set defaults
16110   criterion$="":exclud=1:operator$="=":field_choice=1
16140   MDRAW#8,'select'
16170   MAWDRAW#8,1,f_name$,0,0,,105
16200   REPeat action_loop
16230     action=MCALL(#8; \0)
16260     IF action > 65536 THEN
16290       tempaction=action
16320       FOR i=1 TO FLNUM(#4)
16350         a=MSTAT%(#8,i*65536+1,0)
16380       END FOR i
16410       a=MSTAT%(#8,tempaction,1)
16440       field_choice=MAWNUM(#8,action)
16470     END IF
16500     SELect ON action
16530       =0: MCLEAR#8: RETurn
16560       =-9:a=MSTAT%(#8,-8,0):exclud=1
16590       =-8:a=MSTAT%(#8,-9,0):exclud=0
16620       =-1:a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-3,0):a=MSTAT%(#8,-4,0):operator$="="
16650       =-2:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-3,0):a=MSTAT%(#8,-4,0):operator$="<>"
16680       =-3:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-4,0):operator$="<"
16710       =-4:a=MSTAT%(#8,-1,0):a=MSTAT%(#8,-2,0):a=MSTAT%(#8,-3,0):operator$=">"
16740       =-10:MWINDOW#8,-10:PAPER#8,7:MINPUT#8,criterion$
16770       =-5: MCLEAR #8: EXIT action_loop
16800       =-11: MCLEAR #8: INCLUDE #4: RETurn
16830     END SELect
16860   END REPeat action_loop
16890   IF includ=1 THEN INCLUDE#4;field_choice,operator$,criterion$
16920   IF includ=0 THEN EXCLUDE#4;field_choice,operator$,criterion$
16950   IF COUNT(#4)=0 THEN a=ITEM_SELECT("ERROR","All fields have been excluded","Include all"):INCLUDE #4
16980 END DEFine INCLUDE_FIELDS
17010 :
17040 DEFine PROCedure DELETE_RECORD
17070   LOCal choice
17100   IF COUNT(#4)=1 THEN a=ITEM_SELECT("Warning","You can't delete the last record in a file","OK"):RETurn
17130   choice=ITEM_SELECT('Delete?',"Are you sure you want to delete this record?",yes,no)
17160   IF choice=1 THEN
17190     REMOVE #4
17220   END IF
17250 END DEFine DELETE_RECORD
17280 :
17310 :
17340 :
17370 DEFine PROCedure tabular: REMark new version of tabular edit with PE
17400 LOCal f,r,No_of_fields,no_of_records
17430 temp%=temprecnum
17460 UPDATE_TABLE 0,temp%
17490 REPeat loop_action
17520   act=MCALL(#6; \0)
17550   IF act=-19 THEN
17580      GET_POINTER_POS
17610      IF result%(5)=1 THEN SMALLER
17640      IF result%(5)=2 THEN BIGGER
17670      a=MSTAT%(#6,-19,0)
17700   END IF
17730   IF act>65536 THEN EDIT_TAB
17760   SELect ON act
17790     =0: CLOSE #6:CLOSE #7: er=FDB_CLOSE_DATA(#4):CLAMP:STOP
17820     =-2: MAWCLEAR#6,1: FOR i=-1 TO -13 STEP -1: a=MSTAT%(#6,i,0):NEXT i:RETurn
17850     =-17: ORDER_DATA_BASE:a=MSTAT%(#6,-17,0): changed%=1: UPDATE_TABLE 0,0
17880     =-18: INCLUDE_FIELDS:a=MSTAT%(#6,-18,0):changed%=1:UPDATE_TABLE 0,0
17910   END SELect
17940 END REPeat loop_action
17970 END DEFine
18000 :
18030 DEFine PROCedure EDIT_TAB
18060    LOCal x%,y%,xpos%,ypos%,field$
18090    a=MAWNUM(#6,act,x%,y%)
18120    a=MAWNUM(#6\1,xpos%,ypos%)
18150    REMark PRINT#0, x%,y%
18180    IF y%=0 THEN a=MSTAT%(#6,act,0):RETurn : REMark don't edit field names!
18210    RPOSAB#4,y%-1: field$=FETCH(#4;x%+1): REMark PRINT#0,field$
18240    GET_POINTER_POS
18270    IF result%(5)=1 THEN HOT_STUFF field$: temprecnum=y%-1:a=MSTAT%(#6,act,0): RETurn
18300    MWINDOW #6,1!
18330    INK #6,0
18360    PAPER#6,7
18390    MINPUT#6,field$
18420     f$=field$
18450     typ=f_type(x%)
18480     SELect ON typ
18510       =0:er=FDB_SET(#4;x%+1;f$):
18540       =1:IF NUMBER(f$) AND BOUNDS (f$) THEN f%=f$:er=FDB_SET(#4;x%+1;f%)
18570       =2:IF NUMBER(f$) THEN f=f$:er=FDB_SET(#4;x%+1;f)
18600       =3:IF NUMBER(f$) THEN f=f$:er=FDB_SET(#4;x%+1;f)
18630     END SELect
18660     IF NOT NUMBER(f$) AND typ<>0 THEN REPORT_ERROR -17: changed%=0: UPDATE_TABLE xpos%,ypos%: RETurn
18690     IF type=1 AND NOT BOUNDS(f$) THEN REPORT_ERROR -18: changed%=0:UPDATE_TABLE xpos%,ypos%: RETurn
18720     IF er=-5 THEN REPORT_ERROR -5:changed%=0: UPDATE_TABLE xpos%,ypos%: RETurn
18750     IF er=-17 THEN REPORT_ERROR -17:changed%=0: UPDATE_TABLE xpos%,ypos%:PRINT_ALL_FIELDS: RETurn
18780     temprecnum=y%-1
18810     UPDATE#4
18840     changed%=1:UPDATE_TABLE xpos%,ypos%
18870 END DEFine EDIT_TAB
18900 :
18930 DEFine PROCedure UPDATE_TABLE(x%,y%)
18960   LOCal result%(15)
18990   No_of_fields=FLNUM(#4):no_of_records=COUNT(#4)
19020   IF changed%=1 THEN DIM array$(No_of_fields-1,no_of_records,20)
19050   MWLINK#6,1,#3:MDRAW#6
19080   a=MSTAT%(#6,-1,-1):a=MSTAT%(#6,-2,1):FOR i=-3 TO -13 STEP -1: a=MSTAT%(#6,i,-1):END FOR i
19110   IF changed%=1 THEN
19140   RPOSAB#4,0
19170   FOR i=1 TO No_of_fields
19200     array$(i-1,0)=FLNAME(#4,i)
19230   END FOR i
19260   FOR r=2 TO no_of_records+1
19290     FOR f=1 TO No_of_fields
19320       v$=FETCH(#4,f):IF LEN(v$)>20 THEN v$=v$(1 TO 20)
19350       array$(f-1,r-1)=v$
19380     END FOR f
19410     RPOSAB#4,r-1
19440   END FOR r
19470   END IF
19500   :
19530   REMark work around ugly bug in Easyptr
19560 :
19590   PVAL#6, result%
19620   wheight%=result%(9)
19650   whighchar%=wheight%/12+1
19680   REMark PRINT #0,wheight%,whighchar%
19710   IF COUNT (#4)-y%<whighchar% THEN y%=COUNT(#4)-whighchar%
19740   MAWDRAW#6,1,array$,x%,y%
19770 END DEFine UPDATE_TABLE
19800 :
19830 DEFine FuNction NUMBER(a$)
19860 REMark returns 0 if not numeric, 1 if numeric
19890   LOCal i
19920   nums$="-0123456789."
19950   FOR i=1 TO LEN(a$)
19980      IF NOT (a$(i) INSTR nums$)
20010              RETurn 0
20040      END IF
20070 END FOR i
20100 RETurn 1
20130 END DEFine num
20160 :
20190 DEFine FuNction BOUNDS(a$)
20220  LOCal a
20250  IF NOT NUMBER(a$) THEN RETurn -1
20260  IF a$="" THEN RETurn -1
20280  a=a$
20310  IF a<=32767 AND a > -32768 THEN RETurn 1: ELSE RETurn 0
20340 END DEFine BOUNDS
