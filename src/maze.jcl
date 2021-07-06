//Z85565J JOB 1,NOTIFY=&SYSUID
//***********************************/
//* defs                            */
//***********************************/
//         EXPORT SYMLIST=(SYSUID)
//***********************************/
//* delete dsts                     */
//***********************************/
//DELETE   EXEC PGM=IDCAMS
//SYSPRINT DD   SYSOUT=*
//SYSIN    DD   *,SYMBOLS=JCLONLY
    DELETE &SYSUID..DATA.MAZE.OUT PURGE
    SET MAXCC = 0
/*
//***********************************/
//* compile maze                    */
//***********************************/
//MAZECBL    EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(MAZE),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CBL,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(MAZE),DISP=SHR
//***********************************/
//************* EXEC ****************/
// IF RC = 0 THEN
//***********************************/
//RUN       EXEC PGM=MAZE,PARM=('999','/DEBUG')
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//INFILE    DD *
  xxxxxxxxBx
  xxxxxxxx x
  xxxxxxxx x
  xxxxxxxx x
  xx       x
  xx xxxx xx
  x  x    xx
  x xxx x xx
  x       xx
  xAxxxxxxxx
/*
//OUTFILE   DD DSN=&SYSUID..DATA.MAZE.OUT,
//          DISP=(,CATLG,DELETE),
//          UNIT=SYSDA,
//          LRECL=80,RECFM=FB,
//          SPACE=(80,(1,1),RLSE)
// ELSE
// ENDIF
