unit lowbcm;
//*************************************************************
//Low Level - Functions BCM 2835
//*************************************************************

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Unix;

const
  UART_BASE = $7E20100;


function ReadRegister(Base: pointer; Offset: longint; var ok: boolean): longint;


implementation

function ReadRegister(Base: pointer; Offset: longint; var ok: boolean): longint;
const PAGE_SIZE = 4 *1024;
			BCM270x_PSIZ_Byte= $80000000-$7e000000; // MemoryMap: Size of Peripherals. Docu Page 5

var fd: integer;
     p: ^integer;
begin
  fd:= fpOpen('/dev/mem', O_RDWR or O_SYNC);
  if fd < 0 then begin
  	ok:= false;
    result:= 0;
    exit;
  end;
  //p:= fpmmap(BASE, BLOCK_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED, fd, Offset);
  result:= 0;
  ok:= true;
  fpClose(fd);
end;


end.

