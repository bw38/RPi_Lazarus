unit piSerial;

{
http://wiki.freepascal.org/Multithreaded_Application_Tutorial/de
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, termio, wiringpi, BaseUnix;


type
  TBaudRate=
    (B50, B75, B110, B300, B600, B1200, B2400, B4800,
     B9600, B19200, B38400, B57600, B115200, B230400,
     B460800, B921600);
const
  ConstsBaud: array[TBaudRate] of integer=
    ( 50,  75,  110,  300,  600,  1200,  2400,  4800,
      9600,  19200,  38400,  57600,  115200,  230400,
      460800, 921600);
  //weitere Baudrates -> s. wiringSerial.c

type
  TDataBits=(db8, db7, db6, db5);   //db6 & db5 doesn't work  on UART1
  TParity=(paNone, paOdd, paEven);  //Parity doesn't work  on UART1
  TStopBits=(sbOne,sbTwo); 					//only one Stopbit possible on UART1


TFlowControl=(fcNone,fcHardware);   //xon/xoff not implemented


type
	//Callback type
	TDataReceived = procedure(line: ansistring) of object;


type
TRS232 = class(TThread)

	private
  	Ffd: integer;
    FOnDataReceived: TDataReceived;

    FBaudrate: tBaudRate;
    FDataBits: tDataBits;
    FParity: tParity;
    FStopbits: tStopbits;

    FFlowControl: tFlowControl;
    FRxTimeout : integer;
    FuseSynchronize: boolean; //switch off in Consoleprograms

    rxLine: string;
    procedure DataReceived;

  protected
    procedure Execute; override;

  public
    Device: string;

    property fd: integer read Ffd;  //FileDescriptor
    property Baudrate: tBaudrate read FBaudrate write FBaudrate;
    property DataBits: tDataBits read FDataBits write FDataBits;
    property Parity: tParity read FParity write FParity;
		property Stopbits: tStopbits read FStopbits write FStopbits;

    property FlowControl: tFlowControl read FFLowControl write FFLowControl;
    property RxTimeout: integer read FRxTimeout write FRxTimeout;	//x * 0.1s

    property onDataRcvd: TDataReceived read FOnDataReceived write FOnDataReceived;
    property useSynchronize: boolean write FuseSynchronize;

    constructor create();
    destructor Destroy; override;

    function Open(): integer;
    function WriteData(s: string): integer;
    procedure SuspendTx(active: boolean);

    procedure SetRTS(State: Boolean);
    function  GetCTS(): Boolean;
 end;

implementation

constructor TRS232.create();
begin
  inherited create(false); 	//do not start thread immediatly

  wiringPiSetup();

  Ffd:= -1;
  //Set default PArameters for UART0 115200bd 8N1
  //change before open
  Device:= '/dev/ttyAMA0'; 	//UART0
  Baudrate:= B115200;
  DataBits:= db8;
  Parity:= paNone;
  Stopbits:= sbOne;
  FlowControl:= fcNone;

  RxTimeout:= 10;	//Timeout 1.0sec
  self.FreeOnTerminate := true;
end;

destructor TRS232.Destroy;
begin
  if fd <> -1 then begin
    serialFlush(fd);
    serialClose(fd);
  end;
  inherited Destroy;
end;


function TRS232.Open(): integer;
var bd, res: integer;
  	tios: TermIOS;
    tn: string;
begin
  bd:= ConstsBaud[Baudrate];

  Ffd:= serialOpen(pchar(Device), bd);
  tn:= ttyName(fd);

  //UART has to be activated und Pin-mapped outside !

  if pos('ttyAMA0', tn) > 0 then begin //UART0 map
    if FlowControl = fcHardware then begin
      pinModeAlt(0,  amAlt3); //RTS0
      pinModeAlt(27, amAlt3);	//CTS0
    end;
  end else if pos('ttyS0', tn) > 0 then begin //UART1 map
    if FlowControl = fcHardware then begin
      pinModeAlt(0,  amAlt5); //RTS1
      pinModeAlt(27, amAlt5);	//CTS1
    end;
  end;

  tcgetattr(fd, tios);             //unit termio
  tios.c_cc[VTIME]:= RxTimeout;    //rx timeout  n * 0.1sec

  tios.c_cflag := tios.c_cflag and not CSIZE;
  case DataBits of
    db5: tios.c_cflag:=tios.c_cflag or CS5;
    db6: tios.c_cflag:=tios.c_cflag or CS6;
    db7: tios.c_cflag:=tios.c_cflag or CS7;
    db8: tios.c_cflag:=tios.c_cflag or CS8;
  end;

  //Parity not available on UART1 (mini-UART)
  tios.c_cflag:= tios.c_cflag and not PARENB;		//Parity off
  case Parity of  //default none Parity
    paOdd:  tios.c_cflag:= tios.c_cflag or PARENB or PARODD;  // Enable + odd parity
    paEven: tios.c_cflag:= tios.c_cflag or PARENB;  // Enable + not odd (even) parity
  end;

  //One or Two Stopbits (only one for UART1)
  tios.c_cflag:= tios.c_cflag and not CSTOPB;		//one Stopbit
  if Stopbits = sbTwo then
     tios.c_cflag:= tios.c_cflag or CSTOPB;

  //RTS - CTS - Flowcontrol
  tios.c_cflag:= tios.c_cflag and not CRTSCTS;  //fcNone
  if FlowControl = fcHardware then 
    tios.c_cflag:= tios.c_cflag or CRTSCTS;

  if tcsetattr(fd, TCSANOW, tios) <> 0 then begin  //set immidiately
    result:= -1;
    exit;
  end;
  FuseSynchronize:= true; //switch off only in console-Programs
  Start;						//Start Reading - Thread
  SuspendTx(false); //Don't block Tx on Start
  result:= fd;      //return File-Descrictor
end;

//true blocks Tx until false (independet of Flowcontrol)
procedure TRS232.SuspendTx(active: boolean);
begin
  if fd > 0 then TCFlow(fd, integer(not active));
end;

//Data Receive in Background
procedure TRS232.Execute;
var x: integer;
begin
  while not terminated do begin
    rxLine:= '';
    x:= serialGetchar (fd);  //Blocked for Rx-Timeout div 10
    if (x >= 0) then begin   //-1 for Timeout
      rxLine:= char(x);
      //furthermore characters ?
      while serialDataAvail(fd) > 0 do rxLine:= rxLine + char(serialGetchar (fd));
      //notify Mainthread
      if FuseSynchronize
        then synchronize(@DataReceived)
      	else @DataReceived;
    end;
  end;
end;

//Notifier
procedure TRS232.DataReceived;
begin
  if Assigned(FOnDataReceived) then FOnDataReceived(rxLine);
end;


//Data Send
//write up to 4k data, more will block until written
function TRS232.WriteData(s: string): integer;
begin
  if fd > 0 then result:= fpWrite(fd, pchar(s), length(s));
end;


//Set Status (BCM.17)
// true =>  RTSlow
// false => RTShigh
procedure TRS232.SetRTS(State: Boolean);
const RTS: Cardinal = TIOCM_RTS;
begin
  if State
    then fpioctl(fd, TIOCMBIS, @RTS)
    else fpioctl(fd, TIOCMBIC, @RTS);
end;

//Get Status (BCM.16)
// CTSlow  => true;
// CTShigh => false;
function TRS232.GetCTS(): Boolean;
var Flags: Cardinal;
begin
  fpioctl(fd, TIOCMGET, @Flags);
  Result := (Flags and TIOCM_CTS) <> 0;
end;

end.

