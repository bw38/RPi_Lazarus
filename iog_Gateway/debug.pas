unit debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typedef;

procedure writeDS2cmdline(pl: tPayLoad);

implementation

procedure writeDS2cmdline(pl: tPayLoad);
begin
	writeln('UID:     ' + IntToStr(pl.uid)+#13);
  writeln('Sensors: ' + IntToStr(pl.sensors)+#13);
  writeln('Version: ' + IntToStr(pl.version) + '.' + IntToStr(pl.revision)+#13);
  writeln('---------'+#13);
  writeln('Batterie:' + IntToStr(pl.Batterie)+#13);
  writeln('Runtime: ' + IntToStr(pl.RunTime)+#13);
  writeln('Tx-Shot: ' + IntToStr(pl.cntTxShot)+#13);
  writeln('---------'+#13);
  writeln('Temp:    ' + IntToStr(pl.Temperature)+#13);
  writeln('Status:  ' + IntToStr(pl.StatusTemp)+#13);
  writeln('Res:     ' + IntToStr(pl.ResolutionTemp)+#13);
  writeln('---------'+#13);
  writeln('Level:   ' + InttoStr(pl.Level)+#13);
  writeln('Status:  ' + InttoStr(pl.StatusLevel)+#13);
  writeln('********************'+#13);
end;

end.

