


procedure draw_widget_level(hSize: integer);

const ratio = 1.5; //width = hSize * ratio

var png : TPortableNetworkGraphic;
  w, h, ang, szval, sz, x, y, mxt: integer;
  mcf, pA, pE, pT: TPoint;
  rsA1, rsA2, rsE, rsT, rsN, a: real;
  can: tCanvas;
  rval: TRect;
  tsz: TSize;
  s: string;
  bmp: Graphics.tBitmap;

  //Umrechnung in Prozent bezogen auf GesamthÃ¶he
  function XY(p: real): integer;
  begin
    result:= round(hSize * (p/100));
  end;

begin
  h:= hSize;
  w:= round(hSize * ratio);
  bmp:= Graphics.tBitmap.Create;
  bmp.Clear;
	bmp.Width := w;
	bmp.Height := h;

  mcf.x := XY(50);  //Mittelpunkt Ziffernblatt
  mcf.y := XY(50);

  can:= bmp.Canvas;
  can.AntialiasingMode:=  amOn;
  can.Pen.Color := clBlack;
  can.Pen.Width := 0;
  can.Brush.Color := dsgn.outframe;
  can.Rectangle(0, 0, w-1 , h-1);
  can.Brush.Color := dsgn.outEdge;
  can.Rectangle(XY(2), XY(2), w-XY(2), h-XY(2));
  can.Brush.Color := dsgn.outBack;
  can.Rectangle(XY(4), XY(4), w-XY(4), h-XY(4));
  //Ziffernblatt
  can.Brush.Color := dsgn.outFrame;
  can.Ellipse(mcf.x-XY(42), mcf.y-XY(42), mcf.x +XY(42), mcf.y+XY(42));
  can.Brush.Color := dsgn.clockFace;
  can.Ellipse(mcf.x-XY(39), mcf.y-XY(39), mcf.x +XY(39), mcf.y+XY(39));
  //Skala
  rsA1:= XY(32); 	//Radien Skala
  rsA2:= XY(29);
  rsE:= XY(27);
  rsT:= XY(39);
  rsN:= XY(29);  //Needle

  can.Pen.Color := dsgn.scaleLine;
	can.Pen.Width := XY(1.2);
  can.Font.Size := XY(3.2);
  can.Font.Color := dsgn.scaleText;

  ang:= 0;			//Text Skala
  a:= 0.25*pi; 	//Anfangswinkel
  while ang <= 100 do begin
    if ang mod 2 = 0 then begin
	  	pA.x:= mcf.x - round(rsA1*sin(a));   //lange Striche 10%
  		pA.y:= mcf.y + round(rsA1*cos(a));
    end else begin
      pA.x:= mcf.x - round(rsA2*sin(a));   //kurze Striche 5%
  		pA.y:= mcf.y + round(rsA2*cos(a));
    end;
    pE.x:= mcf.x - round(rsE*sin(a));
  	pE.y:= mcf.y + round(rsE*cos(a));
  	can.Line(pA, pE);

		//Prozentwert Skala 0 / 50 / 100
    case ang of   //mit Feinjustage entspr. TextlÃ¤nge
    	0: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.02*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.02*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      50: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.03*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.03*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      100: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.05*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.05*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
    end; //case

  	a:= a + 0.075*pi;
    ang:= ang + 5;
  end;

  //Feld f. mumerischen Wert im Ziffernblatt
  can.Brush.Color := dsgn.clockValField;
  szval:= XY(20);
  rval.Left := mcf.x - szval div 2;
  rval.Right := rval.Left + szval;

  rval.Top:= mcf.y + XY(20);
  rval.Bottom:= rval.Top + round(szval*0.6);
  can.Rectangle(rval);

  //Numerischer Wert im Ziffernblatt
  s:= IntToStr(level) + ' %';
  can.Font.Size := XY(3.5);
  can.Font.Color := dsgn.clockValTxt;
  can.Font.Orientation := 0;
  can.Brush.Color := dsgn.clockValField;
  tsz:= can.TextExtent(s);
  x:= (rval.Right-rval.Left) div 2 - tsz.cx div 2;
  y:= (rval.Bottom-rval.Top) div 2 - tsz.cy div 2;
  can.Textout(rval.Left+x, rval.Top+y, s);

  //Min- / Max-Level
  can.Pen.Color := dsgn.trend;
  can.Pen.Width:= XY(2.8);
  x:= round(rsN);
  can.Arc(
  	mcf.x - x, mcf.y - x, mcf.x + x, mcf.y + x, //Quadrat des Kreises
  	round(225 * 16 + minlevel * 2.7 * -16),			//auf 0% zurÃ¼ckdrehen (225Â°) - Minwert
    round((maxlevel-minlevel) * 2.7 * -16));		//Winkel Maxwert bezogen auf 270Â° = 100%

  //Nadel
  a:= level*0.015*pi + 0.25*pi;
  can.Pen.Color := dsgn.needle;
  can.Pen.Width := XY(1.8);
  pE.x:= mcf.x - round(rsN*sin(a));   //lange Striche 10%
  pE.y:= mcf.y + round(rsN*cos(a));
  can.Line(mcf, pE);
  can.Brush.Color := dsgn.needle;
  sz:= XY(8) div 2;
  can.Ellipse(mcf.x - sz, mcf.y - sz, mcf.x + sz, mcf.y + sz  );

  //Texte
  mxt:= XY(120); //Centerline der Textausgaben
  can.Brush.Color := dsgn.outBack;
  can.font.Color := dsgn.txt;

  //Name
  can.Font.Size := XY(5);
  tsz:= can.TextExtent(idtxt);
  y:= XY(8);
  can.TextOut(mxt-tsz.cx div 2, y, idtxt);
  //Timestamp
  s:= FormatDateTime('dd.mm.YY - hh:nn:ss', time);
  can.Font.Size := XY(4);;
  tsz:= can.TextExtent(s);
  y:= XY(20);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Temperatur
  s:= 'Temp: ' + FloatToStrF(temp, ffFixed, 7,1) + ' Â°C';
  tsz:= can.TextExtent(s);
  y:= XY(45);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Min-Max-emp
  s:= '[ ' +
  	FloatToStrF(mintemp, ffFixed, 7,1) + ' / ' +
  	FloatToStrF(maxtemp, ffFixed, 7,1) + ' Â°C ]';
  tsz:= can.TextExtent(s);
  y:= XY(55);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Interval
  s:= 'Inteval: ' + IntToStr(interval) + 'min';
  y:= XY(85);
  can.TextOut(mxt-tsz.cx div 2, y, s);

  png:= TPortableNetworkGraphic.Create;
  try
    png.Assign(bmp);
    spng.Clear;
    png.SaveToStream(spng);

  finally
    png.Free;
  end;
  bmp.Free;
end;
