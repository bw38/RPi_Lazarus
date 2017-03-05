unit main;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LazSerial, synaser, wiringpi;



type

  { TForm1 }

  TForm1 = class(TForm)
    btSetup: TSpeedButton;
    Edit1: TEdit;
    Memo1: TMemo;
    btTxLine: TSpeedButton;
    procedure btSetupClick(Sender: TObject);
    procedure btTxLineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
		serial: TLazSerial;
    rxline: string;

    procedure SerialRxData(Sender: TObject);
    procedure StatusChanged(Sender: TObject; Reason: THookSerialReason; const Value: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

//{$DEFINE USBAdapter}
//{$DEFINE UART0}
{$DEFINE UART1}

procedure TForm1.FormCreate(Sender: TObject);
begin
  serial:= TLazSerial.Create(Self);
  {$IFDEF USBAdapter}
  //USB-Serial-Adapter 115200 8N1
  serial.Device := '/dev/ttyUSB0';
  {$ENDIF}
  {$IFDEF UART0}

  {$ENDIF}
  {$IFDEF UART1}
  //UART1 115200 8N1  mini-UART
  //Enable serialport -> 	sudo raspi-config (RxD, TxD is set to Alt5)
  //set Group 				->	sudo chgrp dialout /dev/serial0
  //set permissions 	-> 	sudo chmod 660 /dev/serial0
  serial.Device := '/dev/ttyS0';   //or '/dev/ttyS0'
  {$ENDIF}


  serial.BaudRate := br115200;    //typedef s. lazserial.pas
  serial.DataBits := db8bits;
  serial.Parity := pNone;
  serial.StopBits := sbOne;
  serial.FlowControl := fcNone;
  serial.OnRxData := SerialRxData;
  serial.OnStatus := StatusChanged;
  serial.RcvLineCRLF := false;   //true -> wait Rx for crlf and cut it
  try
  	serial.Open;
  except
    Showmessage('Could not open: ' + serial.Device);
  end;

end;



procedure TForm1.FormDestroy(Sender: TObject);
begin
	serial.Free;
end;

procedure TForm1.SerialRxData(Sender: TObject);
var	i: integer;
  	s: string;
begin
  if serial.DataAvailable then begin
    s:= serial.ReadData;
    for i:= 1 to length(s) do begin
      if s[i] = #10 then begin
        Memo1.Lines.Add(rxline);
        rxline:= '';
      end else begin
      	rxline:= rxline + s[i] ;
      end;
    end;
  end;
end;

procedure TForm1.StatusChanged(Sender: TObject; Reason: THookSerialReason; const Value: string);
begin
  case Reason of    //def.in synaser.pas
    HR_SerialClose: btTxLine.Enabled := false;
  	HR_Connect: 		btTxLine.Enabled := true;
    HR_CanRead:     begin end;
    HR_CanWrite:    begin end;
    HR_ReadCount:   begin end;
    HR_WriteCount:  begin end;
    HR_Wait:        begin end;
  end;
end;

procedure TForm1.btSetupClick(Sender: TObject);
begin
  if serial.Active then serial.Close;
  serial.ShowSetupDialog;
  serial.open;
end;

procedure TForm1.btTxLineClick(Sender: TObject);
begin
  serial.WriteData(edit1.Text + #13#10);
end;

end.

