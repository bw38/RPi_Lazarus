unit main;

{******************************************************************************
Demo for using own Lib rs232.pas on Raspberry for UART0 / UART1
===============================================================

Activate UART1: (/dev/serial0 or /dev/ttyS0
-------------------------------------------
sudo raspi-config -> enable serialport
sudo nano /boot/cmdline.txt
	=> Remove Entry "console=serial0,115200"



Joerg DL7VMD
*******************************************************************************}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, piRS232;



type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    btTxLine: TSpeedButton;
    btSetRTS: TSpeedButton;
    procedure btSetRTSClick(Sender: TObject);
    procedure btTxLineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
		rs232: TRS232;
    rxline: string;

    procedure OpenRS232();

    procedure RxData(s: AnsiString);
    procedure terminatedRS232(Sender: tObject);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin

  OpenRS232();
end;

procedure TForm1.OpenRS232();
var res: integer;
begin
  rs232:= TRS232.create();   					 //UART0 - 115200bd 8N1
  if assigned(rs232) then begin

    rs232.Baudrate := B115200;         // [B115200] Standard-Baudrates s. piRS232.pas
    //rs232.DataBits := db7;					 // [db8], db7, db6, db5
    //rs232.Parity := paEven;          // [paNone], paEven, paOdd
    //rs232.Stopbits := sbTwo;         // [sbOne], sbTow

    //rs232.Device := '/dev/ttyAMA0';  // [UART0] - Bluetooth on Pi3
    rs232.Device := '/dev/ttyS0';    // mini-UART1
    //rs232.Device := '/dev/ttyUSB0';  // external USB-Adapter

    //rs232.FlowControl := fcHardware; // [fcNone], fcHardware

    rs232.onDataRcvd:= RxData;   		 // Rx-event
    rs232.OnTerminate := terminatedRS232;
  	res:= rs232.Open();
    Memo1.Lines.Add('Open Rs232, fd: ' + IntToStr(res));
    btTxLine.Enabled := true;
  end else Memo1.Lines.Add('Error on open Serial-Port');
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
	FreeAndNil(rs232);
end;

// RS232 - Event --------------------------------------------------------------

//Data received
procedure TForm1.RxData(s: AnsiString);
var	i: integer;
begin
  for i:= 1 to length(s) do begin
    if s[i] = #10 then begin
      Memo1.Lines.Add(rxline);
      rxline:= '';
    end else begin
    	rxline:= rxline + s[i] ;
    end;
  end;
end;


//Rx-Thread closed
procedure TForm1.terminatedRS232(Sender: tObject);
begin
  Memo1.Lines.Add('Terminated');
  //...
  rs232:= nil;
end;

//------------------------------------------------------------------------------

procedure TForm1.btTxLineClick(Sender: TObject);
begin
  rs232.WriteData(edit1.Text + #13#10);
  edit1.Text := 'Test: ' + TimeToStr(now);
end;


procedure TForm1.btSetRTSClick(Sender: TObject);
begin
	rs232.SetRTS(btSetRTS.Down);
end;


end.

