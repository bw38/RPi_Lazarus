unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
tDataSet = class

end;

tSensor = class
private
  uid: word;
  typ: word;
  data: tList;
public
  constructor create(uid_, typ_: word);
  destructor destroy;  override;
end;


tSensorList = class

end;

implementation


//------------------------------------------------------------------------------

constructor tSensor.create(uid_, typ_: word);
begin
  inherited create;
  data:= tList.Create;
  uid:= uid_;
  typ:= typ_;


end;

destructor tSensor.destroy;
var i: integer;
begin

  for i:= 0 to data.Count-1 do tDataSet(data[i]).Free;
  data.Free;
  inherited destroy;
end;

end.

